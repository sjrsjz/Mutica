pub mod ast;
pub mod colorize;
pub mod lexer;
pub use ast::TypeAst;
use logos::Logos;
use mutica_core::{
    types::{GcAllocObject, Type},
    util::cycle_detector::FastCycleDetector,
};

use std::{
    collections::HashMap,
    fmt::Debug,
    ops::Deref,
    path::{Path, PathBuf},
    sync::{Arc, RwLock},
};

use crate::{
    grammar::TypeParser,
    parser::{
        ast::{BasicTypeAst, LinearTypeAst},
        colorize::TokenColor,
        lexer::{LexerToken, LexicalError},
    },
    util::{byte_offset_to_char_offset, byte_offset_to_position},
};
use ariadne::{Color, Label, Report, ReportKind};
use lalrpop_util::ErrorRecovery;

/// Calculate the full error span including all dropped tokens
/// Returns (byte_start, byte_end) tuple
pub fn calculate_full_error_span(
    error: &ErrorRecovery<usize, LexerToken, LexicalError>,
) -> (usize, usize) {
    use lalrpop_util::ParseError::*;

    let mut min_pos = usize::MAX;
    let mut max_pos = 0;

    // Get position from the error itself
    match &error.error {
        InvalidToken { location } => {
            min_pos = min_pos.min(*location);
            max_pos = max_pos.max(*location + 1); // At least 1 character
        }
        UnrecognizedToken {
            token: (start, _, end),
            ..
        } => {
            min_pos = min_pos.min(*start);
            max_pos = max_pos.max(*end);
        }
        UnrecognizedEof { location, .. } => {
            min_pos = min_pos.min(*location);
            max_pos = max_pos.max(*location);
        }
        ExtraToken {
            token: (start, _, end),
        } => {
            min_pos = min_pos.min(*start);
            max_pos = max_pos.max(*end);
        }
        User { error } => {
            min_pos = min_pos.min(error.span.start);
            max_pos = max_pos.max(error.span.end);
        }
    }

    // Include all dropped tokens
    for (start, _, end) in &error.dropped_tokens {
        min_pos = min_pos.min(*start);
        max_pos = max_pos.max(*end);
    }

    // Ensure we have a valid range
    if min_pos == usize::MAX {
        (0, 1)
    } else {
        (min_pos, max_pos.max(min_pos + 1))
    }
}

#[derive(Debug, Clone)]
pub enum ParseError<'ast> {
    UseBeforeDeclaration(WithLocation<LinearTypeAst<'ast>>, String),
    RedeclaredCaptureValue(WithLocation<LinearTypeAst<'ast>>, WithLocation<String>),
    UnusedVariable(WithLocation<LinearTypeAst<'ast>>, Vec<WithLocation<String>>),
    AmbiguousPattern(WithLocation<LinearTypeAst<'ast>>),
    PatternOutOfParameterDefinition(WithLocation<LinearTypeAst<'ast>>),
    MissingBranch(WithLocation<LinearTypeAst<'ast>>),
    InternalError(String),
}

impl<'ast> ParseError<'ast> {
    pub fn is_warning(&self) -> bool {
        matches!(self, ParseError::UnusedVariable(_, _))
    }
    /// 辅助函数：从 WithLocation 提取位置信息
    /// 返回 (char_start, char_end, filepath_owned)
    fn extract_location_info(ast: &WithLocation<LinearTypeAst<'ast>>) -> (usize, usize, String) {
        if let Some(location) = ast.location() {
            let source = location.source();
            let span = location.span();
            let content = source.content();
            let char_start = byte_offset_to_char_offset(content, span.start);
            let char_end = byte_offset_to_char_offset(content, span.end);
            let filepath = source.filepath();
            (char_start, char_end, filepath)
        } else {
            // 如果没有位置信息，使用默认值
            (0, 1, "<unknown>".to_string())
        }
    }

    /// 生成一个美观的 ariadne 错误报告
    pub fn report(&self) -> Report<'static, (String, std::ops::Range<usize>)> {
        match self {
            ParseError::UseBeforeDeclaration(ast, name) => {
                let (char_start, char_end, filepath) = Self::extract_location_info(ast);
                Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message(format!("Use of undeclared variable '{}'", name))
                    .with_label(
                        Label::new((filepath, char_start..char_end))
                            .with_message(format!("Variable '{}' is used before declaration", name))
                            .with_color(Color::Red),
                    )
                    .with_help("Make sure the variable is declared before use")
                    .finish()
            }
            ParseError::RedeclaredCaptureValue(ast, name) => {
                // 优先使用 name 的位置信息
                let (report_start, report_filepath) = if let Some(name_location) = name.location() {
                    let source = name_location.source();
                    let span = name_location.span();
                    let content = source.content();
                    let char_start = byte_offset_to_char_offset(content, span.start);
                    (char_start, source.filepath())
                } else {
                    let (char_start, _, filepath) = Self::extract_location_info(ast);
                    (char_start, filepath)
                };

                let mut report =
                    Report::build(ReportKind::Error, report_filepath.clone(), report_start)
                        .with_message(format!("Redeclared capture variable '{}'", name.value()));

                // 如果 name 有位置信息，为其添加 Label
                if let Some(name_location) = name.location() {
                    let source = name_location.source();
                    let span = name_location.span();
                    let content = source.content();
                    let char_start = byte_offset_to_char_offset(content, span.start);
                    let char_end = byte_offset_to_char_offset(content, span.end);
                    let filepath = source.filepath();

                    report = report.with_label(
                        Label::new((filepath, char_start..char_end))
                            .with_message(format!(
                                "Capture variable '{}' is redeclared here",
                                name.value()
                            ))
                            .with_color(Color::Red),
                    );
                }

                // 如果 ast 有位置信息，添加上下文 Label
                if let Some(ast_location) = ast.location() {
                    let source = ast_location.source();
                    let span = ast_location.span();
                    let content = source.content();
                    let ast_start = byte_offset_to_char_offset(content, span.start);
                    let ast_end = byte_offset_to_char_offset(content, span.end);
                    let ast_filepath = source.filepath();

                    report = report.with_label(
                        Label::new((ast_filepath, ast_start..ast_end))
                            .with_message("The variable was already declared in this closure's capture, it might be a internal compiler error")
                            .with_color(Color::Yellow),
                    );
                }

                report
                    .with_help("A pattern cannot contain duplicate variable names")
                    .finish()
            }
            ParseError::UnusedVariable(ast, names) => {
                let var_names: Vec<&str> = names.iter().map(|n| n.value().as_str()).collect();

                // 收集所有有位置信息的变量Label
                let mut labels = Vec::new();
                for name_with_loc in names {
                    if let Some(location) = name_with_loc.location() {
                        let source = location.source();
                        let span = location.span();
                        let content = source.content();
                        let char_start = byte_offset_to_char_offset(content, span.start);
                        let char_end = byte_offset_to_char_offset(content, span.end);
                        let filepath = source.filepath();

                        labels.push(
                            Label::new((filepath, char_start..char_end))
                                .with_message(format!(
                                    "Variable '{}' is declared but never used",
                                    name_with_loc.value()
                                ))
                                .with_color(Color::Yellow),
                        );
                    }
                }

                // 如果没有任何变量有位置信息，使用简化报告
                if labels.is_empty() {
                    let filepath = "<unknown>".to_string();
                    return Report::build(ReportKind::Warning, filepath.clone(), 0)
                        .with_message(format!("Unused variables: {}", var_names.join(", ")))
                        .with_label(
                            Label::new((filepath, 0..1))
                                .with_message("Unable to locate source positions for unused variables")
                                .with_color(Color::Yellow),
                        )
                        .with_help("Consider removing unused variables or prefixing with '_' to intentionally ignore them")
                        .finish();
                }

                // 确定报告的起始位置和文件名（优先使用ast，否则用0）
                let (report_filepath, report_start) = if let Some(ast_location) = ast.location() {
                    let source = ast_location.source();
                    let span = ast_location.span();
                    let content = source.content();
                    let char_start = byte_offset_to_char_offset(content, span.start);
                    (source.filepath(), char_start)
                } else {
                    ("<unknown>".to_string(), 0)
                };

                let mut report =
                    Report::build(ReportKind::Warning, report_filepath.clone(), report_start)
                        .with_message(format!("Unused variables: {}", var_names.join(", ")));

                // 添加所有变量的 Label
                for label in labels {
                    report = report.with_label(label);
                }

                // 如果 ast 有位置信息，添加分析器触发位置的 Label
                if let Some(ast_location) = ast.location() {
                    let source = ast_location.source();
                    let span = ast_location.span();
                    let content = source.content();
                    let ast_start = byte_offset_to_char_offset(content, span.start);
                    let ast_end = byte_offset_to_char_offset(content, span.end);
                    let ast_filepath = source.filepath();

                    report = report.with_label(
                        Label::new((ast_filepath, ast_start..ast_end))
                            .with_message("Analyzer detected unused variables in this scope")
                            .with_color(Color::Cyan),
                    );
                }

                report
                    .with_help("Consider removing unused variables or prefixing with '_' to intentionally ignore them")
                    .finish()
            }
            ParseError::AmbiguousPattern(ast) => {
                let (char_start, char_end, filepath) = Self::extract_location_info(ast);
                Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message("Ambiguous pattern")
                    .with_label(
                        Label::new((filepath, char_start..char_end))
                            .with_message("Here: pattern variables are not allowed within generalized/specialized types. Pattern variables may only be used in ordered contexts (e.g., tuples, lists)")
                            .with_color(Color::Red),
                    )
                    .finish()
            }
            ParseError::PatternOutOfParameterDefinition(ast) => {
                let (char_start, char_end, filepath) = Self::extract_location_info(ast);
                Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message("Pattern definition appears in an invalid location")
                    .with_label(
                        Label::new((filepath, char_start..char_end))
                            .with_message("Patterns can only be used in parameter definitions")
                            .with_color(Color::Red),
                    )
                    .with_help("Pattern variables may only appear in function parameters or match branch bindings")
                    .finish()
            }
            ParseError::MissingBranch(ast) => {
                let (char_start, char_end, filepath) = Self::extract_location_info(ast);
                Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message("Missing required branch")
                    .with_label(
                        Label::new((filepath, char_start..char_end))
                            .with_message("A match expression requires at least one branch")
                            .with_color(Color::Red),
                    )
                    .finish()
            }
            ParseError::InternalError(msg) => {
                // InternalError 没有 AST，使用默认位置
                let filepath = "<unknown>".to_string();
                Report::build(ReportKind::Error, filepath.clone(), 0)
                    .with_message("Internal compiler error")
                    .with_label(
                        Label::new((filepath, 0..1))
                            .with_message(msg.clone())
                            .with_color(Color::Magenta),
                    )
                    .with_note("This is a compiler bug; please report it to the maintainers")
                    .finish()
            }
        }
    }
}

/// 为 lalrpop 的 ErrorRecovery 生成美观的错误报告
pub fn report_error_recovery<'a>(
    error: &ErrorRecovery<usize, LexerToken, LexicalError>,
    filepath: &'a str,
    source: &str,
) -> Report<'a, (&'a str, std::ops::Range<usize>)> {
    use lalrpop_util::ParseError::*;

    // Calculate the full error span including dropped tokens
    let (span_start_byte, span_end_byte) = calculate_full_error_span(error);
    let span_start_char = byte_offset_to_char_offset(source, span_start_byte);
    let span_end_char = byte_offset_to_char_offset(source, span_end_byte);

    match &error.error {
        InvalidToken { location } => {
            let (line, col) = byte_offset_to_position(source, *location);
            let char_offset = byte_offset_to_char_offset(source, *location);
            Report::build(ReportKind::Error, filepath, char_offset)
                .with_message(format!("Invalid token at line {}, column {}", line, col))
                .with_label(
                    Label::new((filepath, span_start_char..span_end_char))
                        .with_message("The token at this position is not recognized")
                        .with_color(Color::Red),
                )
        }
        UnrecognizedToken {
            token: (start, token, end),
            expected,
        } => {
            let (line, col) = byte_offset_to_position(source, *start);
            let char_start = byte_offset_to_char_offset(source, *start);
            let char_end = byte_offset_to_char_offset(source, *end);

            let mut report_builder = Report::build(ReportKind::Error, filepath, char_start)
                .with_message(format!(
                    "Unrecognized token {:?} at line {}, column {}",
                    token, line, col
                ))
                .with_label(
                    Label::new((filepath, char_start..char_end))
                        .with_message({
                            if !expected.is_empty() {
                                let expected_str = expected.join(", ");
                                format!("Expected one of: {}", expected_str)
                            } else {
                                "Invalid token".to_string()
                            }
                        })
                        .with_color(Color::Red),
                );

            // Add a secondary label showing the full error range if dropped tokens exist
            if !error.dropped_tokens.is_empty()
                && (span_start_char < char_start || span_end_char > char_end)
            {
                report_builder = report_builder.with_label(
                    Label::new((filepath, span_start_char..span_end_char))
                        .with_message("Full error region (including skipped tokens)")
                        .with_color(Color::Yellow),
                );
            }

            report_builder
        }
        UnrecognizedEof { location, expected } => {
            let (line, col) = byte_offset_to_position(source, *location);
            let char_offset = byte_offset_to_char_offset(source, *location);
            Report::build(ReportKind::Error, filepath, char_offset)
                .with_message(format!(
                    "Unexpected end of file at line {}, column {}",
                    line, col
                ))
                .with_label(
                    Label::new((filepath, span_start_char..span_end_char.max(1)))
                        .with_message({
                            if !expected.is_empty() {
                                let expected_str = expected.join(", ");
                                format!("Expected one of: {}", expected_str)
                            } else {
                                "Unexpected end of file".to_string()
                            }
                        })
                        .with_color(Color::Red),
                )
        }
        ExtraToken {
            token: (start, token, end),
        } => {
            let (line, col) = byte_offset_to_position(source, *start);
            let char_start = byte_offset_to_char_offset(source, *start);
            let char_end = byte_offset_to_char_offset(source, *end);
            let mut report_builder = Report::build(ReportKind::Error, filepath, char_start)
                .with_message(format!(
                    "Extra token {:?} at line {}, column {}",
                    token, line, col
                ))
                .with_label(
                    Label::new((filepath, char_start..char_end))
                        .with_message("Try removing this token")
                        .with_color(Color::Yellow),
                );

            // Add a secondary label showing the full error range if dropped tokens exist
            if !error.dropped_tokens.is_empty()
                && (span_start_char < char_start || span_end_char > char_end)
            {
                report_builder = report_builder.with_label(
                    Label::new((filepath, span_start_char..span_end_char))
                        .with_message("Full error region (including skipped tokens)")
                        .with_color(Color::Cyan),
                );
            }

            report_builder
        }
        User { error: lex_error } => {
            let (line, col) = byte_offset_to_position(source, lex_error.span.start);
            let char_start = byte_offset_to_char_offset(source, lex_error.span.start);
            let char_end = byte_offset_to_char_offset(source, lex_error.span.end);
            Report::build(ReportKind::Error, filepath, char_start)
                .with_message(format!("Lexical error at line {}, column {}", line, col))
                .with_label(
                    Label::new((filepath, char_start..char_end))
                        .with_message("There is a lexical error here")
                        .with_color(Color::Red),
                )
        }
    }
    .finish()
}

pub struct ParseContext {
    pub declared_variables: Vec<HashMap<String, (usize, WithLocation<()>)>>,
}
pub enum ContextError {
    NotUsed(Vec<WithLocation<String>>),
    NotDeclared(String),
    EmptyContext,
}
impl ParseContext {
    const NOT_USED: usize = 0usize;

    pub fn new() -> Self {
        Self {
            declared_variables: vec![HashMap::new()],
        }
    }

    pub fn capture(&self) -> Vec<WithLocation<String>> {
        let mut captured = Vec::new();
        for scope in &self.declared_variables {
            for (name, (count, loc)) in scope {
                if *count > Self::NOT_USED {
                    captured.push(loc.clone().map(|_| name.clone()));
                }
            }
        }
        captured
    }

    pub fn enter_scope(&mut self) {
        self.declared_variables.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) -> Result<(), ContextError> {
        if let Some(current_scope) = self.declared_variables.last() {
            let unused_vars: Vec<WithLocation<String>> = current_scope
                .iter()
                .filter_map(|(name, (count, loc))| {
                    if *count == Self::NOT_USED && !name.starts_with("_") {
                        Some(loc.clone().map(|_| name.clone()))
                    } else {
                        None
                    }
                })
                .collect();
            if !unused_vars.is_empty() {
                self.declared_variables.pop();
                return Err(ContextError::NotUsed(unused_vars));
            }
        } else {
            return Err(ContextError::EmptyContext);
        }
        self.declared_variables.pop();
        Ok(())
    }

    pub fn declare_variable(
        &mut self,
        name: String,
        loc: Option<&SourceLocation>,
    ) -> Result<(), ContextError> {
        if let Some(current_scope) = self.declared_variables.last_mut() {
            if current_scope.contains_key(&name)
                && current_scope[&name].0 == Self::NOT_USED
                && !name.starts_with("_")
            // 允许以 _ 开头的变量不被使用
            {
                let unused_vars = vec![current_scope[&name].1.clone().map(|_| name.clone())];
                current_scope.insert(name, (Self::NOT_USED, WithLocation::new((), loc)));
                return Err(ContextError::NotUsed(unused_vars));
            }
            current_scope.insert(name, (Self::NOT_USED, WithLocation::new((), loc)));
            return Ok(());
        }
        Err(ContextError::EmptyContext)
    }

    pub fn use_variable(&mut self, name: &str) -> Result<&WithLocation<()>, ContextError> {
        for scope in self.declared_variables.iter_mut().rev() {
            if let Some((count, loc)) = scope.get_mut(name) {
                *count += 1;
                return Ok(loc);
            }
        }
        Err(ContextError::NotDeclared(name.to_string()))
    }
}

pub struct BuildContextLayer<T: GcAllocObject<T, Inner = Type<T>>> {
    fixpoint_mapping: Vec<(String, Type<T>)>,
    pattern_index_mapping: HashMap<String, isize>,
    captured_index_mapping: HashMap<String, isize>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> BuildContextLayer<T> {
    pub fn new() -> Self {
        Self {
            fixpoint_mapping: Vec::new(),
            pattern_index_mapping: HashMap::new(),
            captured_index_mapping: HashMap::new(),
        }
    }

    pub fn enter_fixpoint(&mut self, name: String, t: Type<T>) {
        self.fixpoint_mapping.push((name, t));
    }

    pub fn exit_fixpoint(&mut self) {
        self.fixpoint_mapping.pop();
    }

    pub fn get(&self, name: &str) -> Option<Result<&Type<T>, isize>> {
        for (n, t) in self.fixpoint_mapping.iter().rev() {
            if n == name {
                return Some(Ok(t));
            }
        }
        if let Some(&index) = self.pattern_index_mapping.get(name) {
            return Some(Err(index));
        }
        if let Some(&index) = self.captured_index_mapping.get(name) {
            return Some(Err(index));
        }
        None
    }

    pub fn push_pattern(&mut self, name: String) -> Option<usize> {
        if self.pattern_index_mapping.contains_key(&name) {
            return None;
        }
        let index = self.pattern_index_mapping.len();
        self.pattern_index_mapping.insert(name, index as isize);
        Some(index)
    }

    pub fn push_captured(&mut self, name: String) -> Option<usize> {
        if self.captured_index_mapping.contains_key(&name) {
            return None;
        }
        let index = self.captured_index_mapping.len();
        self.captured_index_mapping
            .insert(name, -1 - index as isize);
        Some(index)
    }

    pub fn captured_count(&self) -> usize {
        self.captured_index_mapping.len()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Default for BuildContextLayer<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct PatternCounter {
    index_mapping: HashMap<String, usize>,
}

impl PatternCounter {
    pub fn new() -> Self {
        Self {
            index_mapping: HashMap::new(),
        }
    }

    pub fn alloc(&mut self, name: String) -> usize {
        if let Some(&index) = self.index_mapping.get(&name) {
            return index;
        }
        let index = self.index_mapping.len();
        self.index_mapping.insert(name, index);
        index
    }
}

pub struct BuildContext<T: GcAllocObject<T, Inner = Type<T>>> {
    layers: Vec<BuildContextLayer<T>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> BuildContext<T> {
    pub fn new() -> Self {
        Self {
            layers: vec![BuildContextLayer::new()],
        }
    }

    pub fn enter_layer(&mut self) {
        self.layers.push(BuildContextLayer::new());
    }

    pub fn exit_layer(&mut self) -> BuildContextLayer<T> {
        self.layers
            .pop()
            .expect("There should always be at least one layer")
    }

    pub fn current_layer_mut(&mut self) -> &mut BuildContextLayer<T> {
        self.layers
            .last_mut()
            .expect("There should always be at least one layer")
    }

    pub fn current_layer(&self) -> &BuildContextLayer<T> {
        self.layers
            .last()
            .expect("There should always be at least one layer")
    }
}

pub struct SourceFile {
    path: Option<PathBuf>,
    content: String,
    color_mapping: RwLock<Vec<TokenColor>>,
}

impl Debug for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "SourceFile {{ path: {:?}, content: <{} bytes> }}",
            self.path,
            self.content.len()
        )
    }
}

impl PartialEq for SourceFile {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path && self.content == other.content
    }
}

impl SourceFile {
    pub fn new(path: Option<PathBuf>, content: String) -> Self {
        Self {
            path,
            color_mapping: RwLock::new(TokenColor::new_buffer(content.len())),
            content,
        }
    }

    pub fn filepath(&self) -> String {
        if let Some(path) = &self.path {
            // 优先返回完整路径，这样 ariadne 的 cache 才能正确匹配
            return path.to_string_lossy().to_string();
        }
        "<input>".to_string()
    }

    pub fn path(&self) -> Option<&PathBuf> {
        self.path.as_ref()
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn color_mapping(&self) -> std::sync::RwLockReadGuard<'_, Vec<TokenColor>> {
        self.color_mapping
            .read()
            .expect("Failed to acquire read lock on color mapping")
    }

    pub fn color_mapping_mut(&self) -> std::sync::RwLockWriteGuard<'_, Vec<TokenColor>> {
        self.color_mapping
            .write()
            .expect("Failed to acquire write lock on color mapping")
    }
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    source: Arc<SourceFile>,
    span: std::ops::Range<usize>, // byte range
}

impl SourceLocation {
    pub fn new(source: Arc<SourceFile>, span: std::ops::Range<usize>) -> Self {
        Self { source, span }
    }

    pub fn source(&self) -> &SourceFile {
        self.source.as_ref()
    }

    pub fn span(&self) -> &std::ops::Range<usize> {
        &self.span
    }
}

#[derive(Clone)]
pub struct WithLocation<T, P = ()>
where
    P: Clone,
{
    value: T,
    location: Option<SourceLocation>,
    payload: P,
}

impl<T, P> Debug for WithLocation<T, P>
where
    T: Debug,
    P: Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

impl<T, P> WithLocation<T, P>
where
    P: Clone + Default,
{
    pub fn new<'a, I: Into<&'a SourceLocation>>(value: T, location: Option<I>) -> Self {
        Self {
            value,
            location: location.map(|l| l.into().clone()),
            payload: Default::default(),
        }
    }
}

impl<T, P> WithLocation<T, P>
where
    P: Clone,
{
    pub fn with_payload(self, payload: P) -> Self {
        Self { payload, ..self }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithLocation<U, P> {
        WithLocation {
            value: f(self.value),
            location: self.location,
            payload: self.payload,
        }
    }

    pub fn as_ref(&self) -> WithLocation<&T, P> {
        WithLocation {
            value: &self.value,
            location: self.location.clone(),
            payload: self.payload.clone(),
        }
    }

    pub fn unwrap(self) -> T {
        self.value
    }

    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn location(&self) -> Option<&SourceLocation> {
        self.location.as_ref()
    }

    pub fn payload(&self) -> &P {
        &self.payload
    }

    pub fn map_payload<Q: Clone + Debug>(self, f: impl FnOnce(P) -> Q) -> WithLocation<T, Q> {
        WithLocation {
            value: self.value,
            location: self.location,
            payload: f(self.payload),
        }
    }
}

impl<T, P> From<T> for WithLocation<T, P>
where
    P: Clone + Default,
{
    fn from(value: T) -> WithLocation<T, P> {
        WithLocation {
            value,
            location: None,
            payload: Default::default(),
        }
    }
}

impl<T, P> Deref for WithLocation<T, P>
where
    P: Clone,
{
    type Target = T;
    fn deref(&self) -> &T {
        &self.value
    }
}

pub struct MultiFileBuilder<'a> {
    imported_ast: &'a mut HashMap<PathBuf, (WithLocation<BasicTypeAst>, Arc<SourceFile>)>,
    path: &'a mut FastCycleDetector<PathBuf>,
    errors: &'a mut Vec<WithLocation<MultiFileBuilderError>>,
}

pub enum MultiFileBuilderError {
    SyntaxError(lalrpop_util::ParseError<usize, LexerToken, LexicalError>),
    RecoveryError(ErrorRecovery<usize, LexerToken, LexicalError>),
    IOError(std::io::Error),
}

impl<'a> MultiFileBuilder<'a> {
    pub fn new(
        imported_ast: &'a mut HashMap<PathBuf, (WithLocation<BasicTypeAst>, Arc<SourceFile>)>,
        path: &'a mut FastCycleDetector<PathBuf>,
        errors: &'a mut Vec<WithLocation<MultiFileBuilderError>>,
    ) -> Self {
        Self {
            imported_ast,
            path,
            errors,
        }
    }

    pub fn build(
        &mut self,
        path: PathBuf,
        code: String,
    ) -> (
        Option<(WithLocation<BasicTypeAst>, Arc<SourceFile>)>,
        Arc<SourceFile>,
    ) {
        let source = Arc::new(SourceFile::new(Some(path.clone()), code));
        let lexer = lexer::LexerToken::lexer(source.content());
        let spanned_lexer = lexer.spanned().map(|(token_result, span)| {
            let token = token_result?;
            Ok((span.start, token, span.end))
        });

        let parser = TypeParser::new();
        let mut color_mapping = source.color_mapping_mut();
        let parse_result = parser.parse(&source, &mut color_mapping, spanned_lexer);
        drop(color_mapping); // 释放可变引用
        let ast = match parse_result {
            Ok(ast) => ast,
            Err(err) => {
                self.errors.push(WithLocation::new(
                    MultiFileBuilderError::SyntaxError(err),
                    Some(&SourceLocation::new(
                        source.clone(),
                        0..source.content().len(),
                    )),
                ));
                return (None, source);
            }
        };
        let mut rec_errors = Vec::new();
        ast.collect_errors(&mut rec_errors);
        for err in rec_errors {
            self.errors.push(WithLocation::new(
                MultiFileBuilderError::RecoveryError(err),
                Some(&SourceLocation::new(
                    source.clone(),
                    0..source.content().len(),
                )),
            ));
        }
        let ast = TypeAst::sanitize(ast);
        // 将路径规范化为绝对路径以保证唯一性
        let canonical_path = path.canonicalize().unwrap_or(path);

        if self.imported_ast.contains_key(&canonical_path) {
            return (self.imported_ast.get(&canonical_path).cloned(), source);
        }
        let result = self
            .path
            .with_guard(canonical_path.clone(), |detector| {
                std::env::set_current_dir(canonical_path.parent().unwrap_or(Path::new("."))).ok();
                let mut new_ctx = MultiFileBuilder {
                    imported_ast: self.imported_ast,
                    path: detector,
                    errors: self.errors,
                };
                let basic_ast = ast.into_basic(&mut new_ctx, ast.location());
                self.imported_ast
                    .insert(canonical_path, (basic_ast.clone(), source.clone()));
                (basic_ast, source.clone())
            })
            .map(|ast| (Some(ast), source.clone()))
            .unwrap_or_else(|| {
                self.errors.push(WithLocation::new(
                    MultiFileBuilderError::IOError(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        "Cyclic import detected",
                    )),
                    Some(&SourceLocation::new(
                        source.clone(),
                        0..source.content().len(),
                    )),
                ));
                (None, source)
            });
        self.path
            .last()
            .map(|path| std::env::set_current_dir(path.parent().unwrap_or(Path::new("."))).ok());
        result
    }
}

pub struct SyntaxError(lalrpop_util::ParseError<usize, LexerToken, LexicalError>);
impl SyntaxError {
    pub fn new(e: lalrpop_util::ParseError<usize, LexerToken, LexicalError>) -> Self {
        SyntaxError(e)
    }

    /// 生成美观的 ariadne 错误报告
    pub fn report(
        &self,
        filepath: String,
        source: &str,
    ) -> Report<'static, (String, std::ops::Range<usize>)> {
        use lalrpop_util::ParseError::*;

        match &self.0 {
            InvalidToken { location } => {
                let char_pos = byte_offset_to_char_offset(source, *location);
                Report::build(ReportKind::Error, filepath.clone(), char_pos)
                    .with_message("Invalid token")
                    .with_label(
                        Label::new((filepath, char_pos..char_pos + 1))
                            .with_message("Invalid token found here")
                            .with_color(Color::Red),
                    )
                    .finish()
            }
            UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => {
                let char_start = byte_offset_to_char_offset(source, *start);
                let char_end = byte_offset_to_char_offset(source, *end);
                let mut report = Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message(format!("Unrecognized token: {:?}", token))
                    .with_label(
                        Label::new((filepath.clone(), char_start..char_end))
                            .with_message("Unexpected token")
                            .with_color(Color::Red),
                    );

                if !expected.is_empty() {
                    report = report.with_help(format!("Expected one of: {}", expected.join(", ")));
                }

                report.finish()
            }
            UnrecognizedEof { location, expected } => {
                let char_pos = byte_offset_to_char_offset(source, *location);
                let mut report = Report::build(ReportKind::Error, filepath.clone(), char_pos)
                    .with_message("Unexpected end of input")
                    .with_label(
                        Label::new((filepath.clone(), char_pos..char_pos))
                            .with_message("Expected more input here")
                            .with_color(Color::Red),
                    );

                if !expected.is_empty() {
                    report = report.with_help(format!("Expected one of: {}", expected.join(", ")));
                }

                report.finish()
            }
            ExtraToken {
                token: (start, token, end),
            } => {
                let char_start = byte_offset_to_char_offset(source, *start);
                let char_end = byte_offset_to_char_offset(source, *end);
                Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message(format!("Extra token: {:?}", token))
                    .with_label(
                        Label::new((filepath, char_start..char_end))
                            .with_message("This token should not be here")
                            .with_color(Color::Red),
                    )
                    .finish()
            }
            User { error } => {
                let char_start = byte_offset_to_char_offset(source, error.span.start);
                let char_end = byte_offset_to_char_offset(source, error.span.end);
                Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message("Lexical error")
                    .with_label(
                        Label::new((filepath, char_start..char_end))
                            .with_message(format!("{:?}", error))
                            .with_color(Color::Red),
                    )
                    .finish()
            }
        }
    }
}
