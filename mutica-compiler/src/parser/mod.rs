pub mod ast;
pub mod lexer;
pub use ast::TypeAst;
use mutica_core::types::Type;

use std::{collections::HashMap, fmt::Debug, ops::Deref, path::PathBuf, sync::Arc};

use crate::{
    parser::{
        ast::LinearTypeAst,
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
    RedeclaredPattern(WithLocation<LinearTypeAst<'ast>>, WithLocation<String>),
    UnusedVariable(WithLocation<LinearTypeAst<'ast>>, Vec<WithLocation<String>>),
    AmbiguousPattern(WithLocation<LinearTypeAst<'ast>>),
    PatternOutOfParameterDefinition(WithLocation<LinearTypeAst<'ast>>),
    MissingBranch(WithLocation<LinearTypeAst<'ast>>),
    InternalError(String),
}

impl<'ast> ParseError<'ast> {
    /// 辅助函数：从 WithLocation 提取位置信息
    /// 返回 (char_start, char_end, filename_owned)
    fn extract_location_info(ast: &WithLocation<LinearTypeAst<'ast>>) -> (usize, usize, String) {
        if let Some(location) = ast.location() {
            let source = location.source();
            let span = location.span();
            let content = source.content();
            let char_start = byte_offset_to_char_offset(content, span.start);
            let char_end = byte_offset_to_char_offset(content, span.end);
            let filename = source.filename();
            (char_start, char_end, filename)
        } else {
            // 如果没有位置信息，使用默认值
            (0, 1, "<unknown>".to_string())
        }
    }

    /// 生成一个美观的 ariadne 错误报告
    pub fn report(&self) -> Report<'static, (String, std::ops::Range<usize>)> {
        match self {
            ParseError::UseBeforeDeclaration(ast, name) => {
                let (char_start, char_end, filename) = Self::extract_location_info(ast);
                Report::build(ReportKind::Error, filename.clone(), char_start)
                    .with_message(format!("Use of undeclared variable '{}'", name))
                    .with_label(
                        Label::new((filename, char_start..char_end))
                            .with_message(format!("Variable '{}' is used before declaration", name))
                            .with_color(Color::Red),
                    )
                    .with_help("Make sure the variable is declared before use")
                    .finish()
            }
            ParseError::RedeclaredPattern(ast, name) => {
                // 优先使用 name 的位置信息
                let (report_start, report_filename) = if let Some(name_location) = name.location() {
                    let source = name_location.source();
                    let span = name_location.span();
                    let content = source.content();
                    let char_start = byte_offset_to_char_offset(content, span.start);
                    (char_start, source.filename())
                } else {
                    let (char_start, _, filename) = Self::extract_location_info(ast);
                    (char_start, filename)
                };

                let mut report =
                    Report::build(ReportKind::Error, report_filename.clone(), report_start)
                        .with_message(format!("Redeclared pattern variable '{}'", name.value()));

                // 如果 name 有位置信息，为其添加 Label
                if let Some(name_location) = name.location() {
                    let source = name_location.source();
                    let span = name_location.span();
                    let content = source.content();
                    let char_start = byte_offset_to_char_offset(content, span.start);
                    let char_end = byte_offset_to_char_offset(content, span.end);
                    let filename = source.filename();

                    report = report.with_label(
                        Label::new((filename, char_start..char_end))
                            .with_message(format!(
                                "Pattern variable '{}' is redeclared here",
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
                    let ast_filename = source.filename();

                    report = report.with_label(
                        Label::new((ast_filename, ast_start..ast_end))
                            .with_message("The variable was already declared in this pattern")
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
                        let filename = source.filename();

                        labels.push(
                            Label::new((filename, char_start..char_end))
                                .with_message(format!(
                                    "Variable '{}' is declared but never used",
                                    name_with_loc.value()
                                ))
                                .with_color(Color::Red),
                        );
                    }
                }

                // 如果没有任何变量有位置信息，使用简化报告
                if labels.is_empty() {
                    let filename = "<unknown>".to_string();
                    return Report::build(ReportKind::Warning, filename.clone(), 0)
                        .with_message(format!("Unused variables: {}", var_names.join(", ")))
                        .with_label(
                            Label::new((filename, 0..1))
                                .with_message("Unable to locate source positions for unused variables")
                                .with_color(Color::Yellow),
                        )
                        .with_help("Consider removing unused variables or prefixing with '_' to intentionally ignore them")
                        .finish();
                }

                // 确定报告的起始位置和文件名（优先使用ast，否则用0）
                let (report_filename, report_start) = if let Some(ast_location) = ast.location() {
                    let source = ast_location.source();
                    let span = ast_location.span();
                    let content = source.content();
                    let char_start = byte_offset_to_char_offset(content, span.start);
                    (source.filename(), char_start)
                } else {
                    ("<unknown>".to_string(), 0)
                };

                let mut report =
                    Report::build(ReportKind::Error, report_filename.clone(), report_start)
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
                    let ast_filename = source.filename();

                    report = report.with_label(
                        Label::new((ast_filename, ast_start..ast_end))
                            .with_message("Analyzer detected unused variables in this scope")
                            .with_color(Color::Cyan),
                    );
                }

                report
                    .with_help("Mutica enforces strict variable usage and any variables should be used at least once")
                    .finish()
            }
            ParseError::AmbiguousPattern(ast) => {
                let (char_start, char_end, filename) = Self::extract_location_info(ast);
                Report::build(ReportKind::Error, filename.clone(), char_start)
                    .with_message("Ambiguous pattern")
                    .with_label(
                        Label::new((filename, char_start..char_end))
                            .with_message("Here: pattern variables are not allowed within generalized/specialized types. Pattern variables may only be used in ordered contexts (e.g., tuples, lists)")
                            .with_color(Color::Red),
                    )
                    .finish()
            }
            ParseError::PatternOutOfParameterDefinition(ast) => {
                let (char_start, char_end, filename) = Self::extract_location_info(ast);
                Report::build(ReportKind::Error, filename.clone(), char_start)
                    .with_message("Pattern definition appears in an invalid location")
                    .with_label(
                        Label::new((filename, char_start..char_end))
                            .with_message("Patterns can only be used in parameter definitions")
                            .with_color(Color::Red),
                    )
                    .with_help("Pattern variables may only appear in function parameters or match branch bindings")
                    .finish()
            }
            ParseError::MissingBranch(ast) => {
                let (char_start, char_end, filename) = Self::extract_location_info(ast);
                Report::build(ReportKind::Error, filename.clone(), char_start)
                    .with_message("Missing required branch")
                    .with_label(
                        Label::new((filename, char_start..char_end))
                            .with_message("A match expression requires at least one branch")
                            .with_color(Color::Red),
                    )
                    .finish()
            }
            ParseError::InternalError(msg) => {
                // InternalError 没有 AST，使用默认位置
                let filename = "<unknown>".to_string();
                Report::build(ReportKind::Error, filename.clone(), 0)
                    .with_message("Internal compiler error")
                    .with_label(
                        Label::new((filename, 0..1))
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
pub fn report_error_recovery<'input, 'a>(
    error: &ErrorRecovery<usize, LexerToken<'input>, LexicalError>,
    filename: &'a str,
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
            Report::build(ReportKind::Error, filename, char_offset)
                .with_message(format!("Invalid token at line {}, column {}", line, col))
                .with_label(
                    Label::new((filename, span_start_char..span_end_char))
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

            let mut report_builder = Report::build(ReportKind::Error, filename, char_start)
                .with_message(format!(
                    "Unrecognized token {:?} at line {}, column {}",
                    token, line, col
                ))
                .with_label(
                    Label::new((filename, char_start..char_end))
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
                    Label::new((filename, span_start_char..span_end_char))
                        .with_message("Full error region (including skipped tokens)")
                        .with_color(Color::Yellow),
                );
            }

            report_builder
        }
        UnrecognizedEof { location, expected } => {
            let (line, col) = byte_offset_to_position(source, *location);
            let char_offset = byte_offset_to_char_offset(source, *location);
            Report::build(ReportKind::Error, filename, char_offset)
                .with_message(format!(
                    "Unexpected end of file at line {}, column {}",
                    line, col
                ))
                .with_label(
                    Label::new((filename, span_start_char..span_end_char.max(1)))
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
            let mut report_builder = Report::build(ReportKind::Error, filename, char_start)
                .with_message(format!(
                    "Extra token {:?} at line {}, column {}",
                    token, line, col
                ))
                .with_label(
                    Label::new((filename, char_start..char_end))
                        .with_message("Try removing this token")
                        .with_color(Color::Yellow),
                );

            // Add a secondary label showing the full error range if dropped tokens exist
            if !error.dropped_tokens.is_empty()
                && (span_start_char < char_start || span_end_char > char_end)
            {
                report_builder = report_builder.with_label(
                    Label::new((filename, span_start_char..span_end_char))
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
            Report::build(ReportKind::Error, filename, char_start)
                .with_message(format!("Lexical error at line {}, column {}", line, col))
                .with_label(
                    Label::new((filename, char_start..char_end))
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
                    if *count == Self::NOT_USED {
                        Some(loc.clone().map(|_| name.clone()))
                    } else {
                        None
                    }
                })
                .collect();
            if !unused_vars.is_empty() {
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
            if current_scope.contains_key(&name) && current_scope[&name].0 == Self::NOT_USED {
                return Err(ContextError::NotUsed(vec![
                    current_scope[&name].1.clone().map(|_| name),
                ]));
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

pub struct BuildContextLayer {
    fixpoint_mapping: Vec<(String, Type)>,
    pattern_index_mapping: HashMap<String, isize>,
    captured_index_mapping: HashMap<String, isize>,
    pattern_count: usize,
}

impl BuildContextLayer {
    pub fn new() -> Self {
        Self {
            fixpoint_mapping: Vec::new(),
            pattern_index_mapping: HashMap::new(),
            captured_index_mapping: HashMap::new(),
            pattern_count: 0,
        }
    }

    pub fn enter_fixpoint(&mut self, name: String, t: Type) {
        self.fixpoint_mapping.push((name, t));
    }

    pub fn exit_fixpoint(&mut self) {
        self.fixpoint_mapping.pop();
    }

    pub fn get(&self, name: &str) -> Option<Result<&Type, isize>> {
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

    pub fn pattern_count(&self) -> usize {
        self.pattern_index_mapping.len()
    }

    pub fn captured_count(&self) -> usize {
        self.captured_index_mapping.len()
    }

    pub fn inc_pattern_count(&mut self) -> usize {
        self.pattern_count += 1;
        self.pattern_count - 1
    }
}

impl Default for BuildContextLayer {
    fn default() -> Self {
        Self::new()
    }
}

pub struct BuildContext {
    layers: Vec<BuildContextLayer>,
}

impl BuildContext {
    pub fn new() -> Self {
        Self {
            layers: vec![BuildContextLayer::new()],
        }
    }

    pub fn enter_layer(&mut self) {
        self.layers.push(BuildContextLayer::new());
    }

    pub fn exit_layer(&mut self) -> BuildContextLayer {
        self.layers
            .pop()
            .expect("There should always be at least one layer")
    }

    pub fn current_layer_mut(&mut self) -> &mut BuildContextLayer {
        self.layers
            .last_mut()
            .expect("There should always be at least one layer")
    }

    pub fn current_layer(&self) -> &BuildContextLayer {
        self.layers
            .last()
            .expect("There should always be at least one layer")
    }
}

#[derive(Debug)]
pub struct SourceFile {
    path: Option<PathBuf>,
    content: String,
}

impl PartialEq for SourceFile {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path && self.content == other.content
    }
}

impl SourceFile {
    pub fn new(path: Option<PathBuf>, content: String) -> Self {
        Self { path, content }
    }

    pub fn filename(&self) -> String {
        if let Some(path) = &self.path {
            if let Some(name) = path.file_name() {
                if let Some(name_str) = name.to_str() {
                    return name_str.to_string();
                }
            }
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

#[derive(Debug, Clone)]
pub struct WithLocation<T, P = ()>
where
    P: Clone + Debug,
{
    value: T,
    location: Option<SourceLocation>,
    payload: P,
}

impl<T, P> WithLocation<T, P>
where
    P: Clone + Debug + Default,
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
    P: Clone + Debug,
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
    P: Clone + Debug + Default,
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
    P: Clone + Debug + Default,
{
    type Target = T;
    fn deref(&self) -> &T {
        &self.value
    }
}
