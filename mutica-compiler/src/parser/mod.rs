pub mod ast;
pub mod lexer;
pub use ast::TypeAst;
use logos::Logos;
use mutica_core::{
    types::{
        CoinductiveType, GcAllocObject, Type, pattern::Pattern, unify::EnvironmentVarState,
        variable::Variable,
    },
    util::{
        cycle_detector::FastCycleDetector,
        source_info::{
            SourceFile, SourceLocation, byte_offset_to_char_offset, byte_offset_to_position,
        },
    },
};

use std::{
    collections::HashMap,
    fmt::Debug,
    ops::Deref,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    grammar::TypeParser,
    parser::{
        ast::{BasicGenericPattern, BasicTypeAst, LinearTypeAst},
        lexer::{LexerToken, LexicalError},
    },
};
use lalrpop_util::ErrorRecovery;
use mutica_core::ariadne::{Color, Label, Report, ReportKind};

/// Helper function to simplify WithLocation creation with source location
#[inline]
pub fn with_loc<T>(
    value: T,
    src: &Arc<SourceFile>,
    range: std::ops::Range<usize>,
) -> WithLocation<T> {
    WithLocation::new(value, Some(SourceLocation::new(src.clone(), range)).as_ref())
}

pub fn with_no_loc<T>(value: T) -> WithLocation<T> {
    WithLocation::new(value, None::<&SourceLocation>)
}

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
        UnrecognizedToken { token: (start, _, end), .. } => {
            min_pos = min_pos.min(*start);
            max_pos = max_pos.max(*end);
        }
        UnrecognizedEof { location, .. } => {
            min_pos = min_pos.min(*location);
            max_pos = max_pos.max(*location);
        }
        ExtraToken { token: (start, _, end) } => {
            min_pos = min_pos.min(*start);
            max_pos = max_pos.max(*end);
        }
        User { error } => {
            min_pos = min_pos.min(error.span().start);
            max_pos = max_pos.max(error.span().end);
        }
    }

    // Include all dropped tokens
    for (start, _, end) in &error.dropped_tokens {
        min_pos = min_pos.min(*start);
        max_pos = max_pos.max(*end);
    }

    // Ensure we have a valid range
    if min_pos == usize::MAX { (0, 1) } else { (min_pos, max_pos.max(min_pos + 1)) }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UseBeforeDeclaration(WithLocation<LinearTypeAst>, WithLocation<String>),
    RedeclaredCaptureValue(WithLocation<LinearTypeAst>, WithLocation<String>),
    UnusedVariable(WithLocation<LinearTypeAst>, Vec<WithLocation<String>>),
    AmbiguousPattern(WithLocation<LinearTypeAst>),
    PatternOutOfParameterDefinition(WithLocation<LinearTypeAst>),
    MissingBranch(WithLocation<LinearTypeAst>),
    OutgoingFixPointReference(WithLocation<LinearTypeAst>, WithLocation<String>, usize),
    AstNotDesugared(WithLocation<BasicTypeAst>),
    InternalError(String),
}

impl ParseError {
    pub fn is_warning(&self) -> bool {
        matches!(self, ParseError::UnusedVariable(_, _))
    }

    /// 辅助函数：从 WithLocation 提取位置信息
    /// 返回 (char_start, char_end, filepath_owned)
    fn extract_location_info<T>(with_loc: &WithLocation<T>) -> (usize, usize, String) {
        if let Some(location) = with_loc.location() {
            let source = location.source();
            let span = location.span();
            let content = source.content();
            let char_start = byte_offset_to_char_offset(content, span.start);
            let char_end = byte_offset_to_char_offset(content, span.end);
            let filepath = source.filepath();
            (char_start, char_end, filepath)
        } else {
            (0, 1, "<unknown>".to_string())
        }
    }

    /// 生成一个美观的 ariadne 错误报告
    pub fn report(&self) -> Report<'static, (String, std::ops::Range<usize>)> {
        match self {
            ParseError::UseBeforeDeclaration(ast, name) => {
                let (char_start, char_end, filepath) = Self::extract_location_info(name);
                let (ast_char_start, ast_char_end, ast_filepath) = Self::extract_location_info(ast);
                Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message(format!("Use of undeclared variable '{}'", name.value()))
                    .with_label(
                        Label::new((filepath, char_start..char_end))
                            .with_message(format!(
                                "Variable '{}' is used before declaration",
                                name.value()
                            ))
                            .with_color(Color::Red),
                    )
                    .with_label(
                        Label::new((ast_filepath, ast_char_start..ast_char_end))
                            .with_message("Here is where the variable is referenced")
                            .with_color(Color::Cyan),
                    )
                    .with_help("Make sure the variable is declared before use")
                    .finish()
            }
            ParseError::OutgoingFixPointReference(ast, name, count) => {
                let (char_start, _, filepath) = Self::extract_location_info(ast);
                let (var_start, var_end, var_filepath) = Self::extract_location_info(name);

                Report::build(ReportKind::Error, filepath, char_start)
                    .with_message(format!(
                        "Fix-point variable '{}' referenced from {} layer(s) outside function scope",
                        name.value(), count
                    ))
                    .with_label(
                        Label::new((var_filepath, var_start..var_end))
                            .with_message(format!(
                                "Here, the fix-point variable '{}' is used outside its defining function's scope",
                                name.value()
                            ))
                            .with_color(Color::Red),
                    )
                    .with_help("Ensure that fix-point variables are only used within their defining function's scope, or use 'dyn_rec' for dynamic recursion")
                    .finish()
            }
            ParseError::AstNotDesugared(ast) => {
                let (char_start, char_end, filepath) = Self::extract_location_info(ast);
                Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message("CRITICAL: AST node not desugared before type processing")
                    .with_label(
                        Label::new((filepath, char_start..char_end))
                            .with_message(
                                "This AST node should have been desugared before type processing",
                            )
                            .with_color(Color::Red),
                    )
                    .with_help("This is likely a compiler bug; please report it to the maintainers")
                    .finish()
            }
            ParseError::RedeclaredCaptureValue(ast, name) => {
                let (name_start, name_end, name_filepath) = Self::extract_location_info(name);
                let (ast_start, ast_end, ast_filepath) = Self::extract_location_info(ast);

                Report::build(ReportKind::Error, name_filepath.clone(), name_start)
                    .with_message(format!("Redeclared capture variable '{}'", name.value()))
                    .with_label(
                        Label::new((name_filepath, name_start..name_end))
                            .with_message(format!(
                                "Capture variable '{}' is redeclared here",
                                name.value()
                            ))
                            .with_color(Color::Red),
                    )
                    .with_label(
                        Label::new((ast_filepath, ast_start..ast_end))
                            .with_message("The variable was already declared in this closure's capture, it might be a internal compiler error")
                            .with_color(Color::Yellow),
                    )
                    .with_help("A pattern cannot contain duplicate variable names")
                    .finish()
            }
            ParseError::UnusedVariable(ast, names) => {
                let var_names: Vec<&str> = names.iter().map(|n| n.value().as_str()).collect();

                // 收集所有变量的 Label
                let labels: Vec<_> = names
                    .iter()
                    .map(|name_with_loc| {
                        let (char_start, char_end, filepath) =
                            Self::extract_location_info(name_with_loc);
                        Label::new((filepath, char_start..char_end))
                            .with_message(format!(
                                "Variable '{}' is declared but never used",
                                name_with_loc.value()
                            ))
                            .with_color(Color::Yellow)
                    })
                    .collect();

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
                        .with_help(
                            "Consider removing unused variables or prefixing with '_' to intentionally ignore them",
                        )
                        .finish();
                }

                // 确定报告的起始位置和文件名
                let (ast_start, ast_end, ast_filepath) = Self::extract_location_info(ast);

                let mut report =
                    Report::build(ReportKind::Warning, ast_filepath.clone(), ast_start)
                        .with_message(format!("Unused variables: {}", var_names.join(", ")));

                // 添加所有变量的 Label
                for label in labels {
                    report = report.with_label(label);
                }

                // 添加分析器触发位置的 Label
                report = report.with_label(
                    Label::new((ast_filepath, ast_start..ast_end))
                        .with_message("Analyzer detected unused variables in this scope")
                        .with_color(Color::Cyan),
                );

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
                            .with_message("Here: pattern variables are not allowed within AnyOfd/AllOfd types. Pattern variables may only be used in ordered contexts (e.g., tuples, lists)")
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
    filepath: String,
    source: &str,
) -> Report<'a, (String, std::ops::Range<usize>)> {
    SyntaxError::new(error.error.clone()).report(
        filepath.to_string(),
        source,
        Some(calculate_full_error_span(error)),
    )
}

pub enum Scope {
    Function(HashMap<String, (usize, WithLocation<()>)>),
    Generic(HashMap<String, (usize, WithLocation<()>)>),
    FixPoint(String, usize, WithLocation<()>),
}

pub struct ParseContext {
    pub declared_variables: Vec<Scope>,
}
pub enum ContextError {
    NotUsed(Vec<WithLocation<String>>),
    NotDeclared(String),
    EmptyContext,
}
impl Default for ParseContext {
    fn default() -> Self {
        Self::new()
    }
}

impl ParseContext {
    const NOT_USED: usize = 0usize;

    pub fn new() -> Self {
        Self { declared_variables: vec![Scope::Function(HashMap::new())] }
    }

    pub fn capture(&self) -> Vec<WithLocation<String>> {
        let mut captured = Vec::new();
        for scope in &self.declared_variables {
            match scope {
                Scope::Function(map) | Scope::Generic(map) => {
                    for (name, (count, loc)) in map {
                        if *count > Self::NOT_USED {
                            captured.push(loc.clone().map(|_| name.clone()));
                        }
                    }
                }
                Scope::FixPoint(name, count, loc) => {
                    if *count > Self::NOT_USED {
                        captured.push(loc.clone().map(|_| name.clone()));
                    }
                }
            }
        }
        captured
    }

    pub fn enter_scope(&mut self) {
        self.declared_variables.push(Scope::Function(HashMap::new()));
    }

    pub fn enter_generic_scope(&mut self) {
        self.declared_variables.push(Scope::Generic(HashMap::new()));
    }

    pub fn enter_fixpoint_scope(&mut self, name: WithLocation<String>) {
        self.declared_variables.push(Scope::FixPoint(
            name.value().clone(),
            Self::NOT_USED,
            WithLocation::new((), name.location()),
        ));
    }

    pub fn exit_scope(&mut self) -> Result<(), ContextError> {
        if let Some(current_scope) = self.declared_variables.last() {
            let unused_vars: Vec<WithLocation<String>> = match current_scope {
                Scope::Function(map) | Scope::Generic(map) => map
                    .iter()
                    .filter_map(|(name, (count, loc))| {
                        if *count == Self::NOT_USED && !name.starts_with("_") && !name.contains("#")
                        {
                            Some(loc.clone().map(|_| name.clone()))
                        } else {
                            None
                        }
                    })
                    .collect(),
                Scope::FixPoint(name, count, loc) => {
                    if *count == Self::NOT_USED && !name.starts_with("_") && !name.contains("#") {
                        vec![loc.clone().map(|_| name.clone())]
                    } else {
                        Vec::new()
                    }
                }
            };
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

    pub fn declare_variable(&mut self, name: WithLocation<String>) -> Result<(), ContextError> {
        if let Some(current_scope) = self.declared_variables.last_mut() {
            match current_scope {
                Scope::Function(map) | Scope::Generic(map) => {
                    if map.contains_key(name.value())
                        && map[name.value()].0 == Self::NOT_USED
                        && !name.value().starts_with("_")
                        && !name.value().contains("#")
                    // 允许以 _ 开头的变量不被使用
                    {
                        let unused_vars =
                            vec![map[name.value()].1.clone().map(|_| name.value().clone())];
                        map.insert(
                            name.value().clone(),
                            (Self::NOT_USED, WithLocation::new((), name.location())),
                        );
                        return Err(ContextError::NotUsed(unused_vars));
                    }
                    map.insert(
                        name.value().clone(),
                        (Self::NOT_USED, WithLocation::new((), name.location())),
                    );
                    return Ok(());
                }
                Scope::FixPoint(_, _, _) => return Err(ContextError::EmptyContext),
            }
        }
        Err(ContextError::EmptyContext)
    }

    /// 使用变量，返回变量的位置信息和是否为真正的变量还是递归点
    pub fn use_variable(
        &mut self,
        name: &str,
    ) -> Result<(&WithLocation<()>, Option<usize>), ContextError> {
        let mut outgoing_function_layer_count = 0;
        let mut skip_generic = false;
        for scope in self.declared_variables.iter_mut().rev() {
            match scope {
                Scope::Function(map) => {
                    skip_generic = true; // 跨越 Function 层时跳过 Generic 层
                    if let Some((count, loc)) = map.get_mut(name) {
                        *count += 1;
                        return Ok((loc, None));
                    }
                    outgoing_function_layer_count += 1;
                }
                Scope::Generic(map) => {
                    if skip_generic {
                        continue;
                    }
                    if let Some((count, loc)) = map.get_mut(name) {
                        *count += 1;
                        return Ok((loc, None));
                    }
                }
                Scope::FixPoint(n, count, loc) => {
                    if n == name {
                        *count += 1;
                        return Ok((loc, Some(outgoing_function_layer_count)));
                    }
                }
            }
        }
        Err(ContextError::NotDeclared(name.to_string()))
    }
}

pub enum BuildContextLayer<T: GcAllocObject<T, Inner = Type<T>>> {
    Function {
        patterns: HashMap<String, WithLocation<()>>,
        captures: HashMap<String, WithLocation<()>>,
    },
    GenericBinding(HashMap<String, WithLocation<()>>, bool), // 第二个参数用于区分定义/使用
    FixPoint(WithLocation<String>, Type<T>),
}

impl<T: GcAllocObject<T, Inner = Type<T>>> BuildContextLayer<T> {
    pub fn new_function_layer(
        patterns: HashMap<String, WithLocation<()>>,
        captures: HashMap<String, WithLocation<()>>,
    ) -> Self {
        Self::Function { patterns, captures }
    }

    pub fn new_generic_binding_layer(
        patterns: HashMap<String, WithLocation<()>>,
        is_definition: bool,
    ) -> Self {
        Self::GenericBinding(patterns, is_definition)
    }

    pub fn new_fixpoint_layer(name: WithLocation<String>, ty: Type<T>) -> Self {
        Self::FixPoint(name, ty)
    }
}

pub struct BuildContext<T: GcAllocObject<T, Inner = Type<T>>> {
    layers: Vec<BuildContextLayer<T>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Default for BuildContext<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> BuildContext<T> {
    pub fn new() -> Self {
        Self { layers: vec![] }
    }

    pub fn enter_layer(&mut self, layer: BuildContextLayer<T>) {
        self.layers.push(layer)
    }

    pub fn exit_layer(&mut self) -> Option<BuildContextLayer<T>> {
        self.layers.pop()
    }

    pub fn lookup<S: AsRef<str>>(&self, var: S) -> Option<(Type<T>, Option<usize>)> {
        let mut outgoing_function_layer_count = 0;
        let mut skip_generic = false;
        let mut is_last_generic_layer = true;
        for (i, layer) in self.layers.iter().rev().enumerate() {
            match layer {
                BuildContextLayer::Function { patterns, captures } => {
                    skip_generic = true; // 跨越 Function 层时跳过 Generic 层
                    match (patterns.get(var.as_ref()), captures.get(var.as_ref())) {
                        (Some(v), _) => {
                            return Some((
                                Variable::new_argument(
                                    Arc::from(var.as_ref()),
                                    v.location().cloned().map(Arc::new),
                                ),
                                None,
                            ));
                        }
                        (None, Some(v)) => {
                            return Some((
                                Variable::new_context(
                                    Arc::from(var.as_ref()),
                                    v.location().cloned().map(Arc::new),
                                ),
                                None,
                            ));
                        }
                        _ => {}
                    }
                    outgoing_function_layer_count += 1;
                }
                BuildContextLayer::GenericBinding(patterns, is_definition) => {
                    if skip_generic {
                        continue;
                    }
                    if let Some(v) = patterns.get(var.as_ref()) {
                        let mut layer = 0;
                        // 向上查找有多少层 GenericBinding
                        for l in self.layers.iter().rev().skip(i + 1) {
                            match l {
                                BuildContextLayer::GenericBinding(_, _) => {
                                    layer += 1;
                                }
                                BuildContextLayer::FixPoint(_, _) => continue,
                                BuildContextLayer::Function { .. } => break,
                            }
                        }

                        // 如果是定义且是最内层 GenericBinding，则创建 Pattern 类型
                        if *is_definition {
                            if is_last_generic_layer {
                                return Some((
                                    Pattern::new(
                                        Arc::from(var.as_ref()),
                                        layer,
                                        v.location().cloned().map(Arc::new),
                                    )
                                    .dispatch(),
                                    None,
                                ));
                            }
                            // 不是最内层定义，意味着它尝试使用一个未解构的泛型变量，认为它未定义
                        } else {
                            return Some((
                                Variable::new_pattern(
                                    Arc::from(var.as_ref()),
                                    layer,
                                    v.location().cloned().map(Arc::new),
                                )
                                .dispatch(),
                                None,
                            ));
                        }
                    }
                    is_last_generic_layer = false;
                }
                BuildContextLayer::FixPoint(name, v) => {
                    if var.as_ref().eq(name.value()) {
                        return Some((v.clone(), Some(outgoing_function_layer_count)));
                    }
                }
            }
        }
        None
    }

    pub fn lookup_function_env<S: AsRef<str>>(
        &self,
        var: S,
    ) -> Option<EnvironmentVarState<Type<T>, T>> {
        for layer in self.layers.iter().rev() {
            match layer {
                BuildContextLayer::Function { patterns, captures } => {
                    if patterns.contains_key(var.as_ref()) {
                        return Some(EnvironmentVarState::FromArgument);
                    }
                    if captures.contains_key(var.as_ref()) {
                        return Some(EnvironmentVarState::FromCapture);
                    }
                    return None;
                }
                BuildContextLayer::FixPoint(_, _) | BuildContextLayer::GenericBinding(_, _) => {}
            }
        }
        None
    }
}

pub struct WithLocation<T, P = ()>
where
    P: Clone,
{
    value: T,
    location: Option<SourceLocation>,
    payload: P,
}

impl<T, P> Clone for WithLocation<T, P>
where
    T: Clone,
    P: Clone,
{
    #[stacksafe::stacksafe]
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            location: self.location.clone(),
            payload: self.payload.clone(),
        }
    }
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
        Self { value, location: location.map(|l| l.into().clone()), payload: Default::default() }
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
        WithLocation { value: f(self.value), location: self.location, payload: self.payload }
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

    pub fn take_value(self) -> T {
        self.value
    }

    pub fn location(&self) -> Option<&SourceLocation> {
        self.location.as_ref()
    }

    pub fn payload(&self) -> &P {
        &self.payload
    }

    pub fn map_payload<Q: Clone + Debug>(self, f: impl FnOnce(P) -> Q) -> WithLocation<T, Q> {
        WithLocation { value: self.value, location: self.location, payload: f(self.payload) }
    }
}

impl<T, P> From<T> for WithLocation<T, P>
where
    P: Clone + Default,
{
    fn from(value: T) -> WithLocation<T, P> {
        WithLocation { value, location: None, payload: Default::default() }
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
    TopLevelBindError(WithLocation<String>), // Variable with location that has unexpected bind at top level
}

impl<'a> MultiFileBuilder<'a> {
    pub fn new(
        imported_ast: &'a mut HashMap<PathBuf, (WithLocation<BasicTypeAst>, Arc<SourceFile>)>,
        path: &'a mut FastCycleDetector<PathBuf>,
        errors: &'a mut Vec<WithLocation<MultiFileBuilderError>>,
    ) -> Self {
        Self { imported_ast, path, errors }
    }

    #[allow(clippy::type_complexity)]
    #[stacksafe::stacksafe]
    pub fn build(
        &mut self,
        path: PathBuf,
        code: String,
    ) -> (Option<(WithLocation<BasicTypeAst>, Arc<SourceFile>)>, Arc<SourceFile>) {
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
                    Some(&SourceLocation::new(source.clone(), 0..source.content().len())),
                ));
                return (None, source);
            }
        };
        let mut rec_errors = Vec::new();
        ast.collect_errors(&mut rec_errors);
        for err in rec_errors {
            self.errors.push(WithLocation::new(
                MultiFileBuilderError::RecoveryError(err),
                Some(&SourceLocation::new(source.clone(), 0..source.content().len())),
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
                self.imported_ast.insert(canonical_path, (basic_ast.clone(), source.clone()));
                (basic_ast, source.clone())
            })
            .map(|ast| (Some(ast), source.clone()))
            .unwrap_or_else(|| {
                self.errors.push(WithLocation::new(
                    MultiFileBuilderError::IOError(std::io::Error::other("Cyclic import detected")),
                    Some(&SourceLocation::new(source.clone(), 0..source.content().len())),
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
        span: Option<(usize, usize)>,
    ) -> Report<'static, (String, std::ops::Range<usize>)> {
        use lalrpop_util::ParseError::*;

        match &self.0 {
            InvalidToken { location } => {
                let (line, col) = byte_offset_to_position(source, *location);
                let char_offset = byte_offset_to_char_offset(source, *location);
                let (span_start_char, span_end_char) = match span {
                    Some(span) => (
                        byte_offset_to_char_offset(source, span.0),
                        byte_offset_to_char_offset(source, span.1),
                    ),
                    None => (char_offset, char_offset + 1),
                };
                Report::build(ReportKind::Error, filepath.clone(), char_offset)
                    .with_message(format!("Invalid token at line {}, column {}", line, col))
                    .with_label(
                        Label::new((filepath, span_start_char..span_end_char))
                            .with_message("The token at this position is not recognized")
                            .with_color(Color::Red),
                    )
            }
            UnrecognizedToken { token: (start, token, end), expected } => {
                let (line, col) = byte_offset_to_position(source, *start);
                let char_start = byte_offset_to_char_offset(source, *start);
                let char_end = byte_offset_to_char_offset(source, *end);
                let (span_start_char, span_end_char) = match span {
                    Some(span) => (
                        byte_offset_to_char_offset(source, span.0),
                        byte_offset_to_char_offset(source, span.1),
                    ),
                    None => (char_start, char_end),
                };
                Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message(format!(
                        "Unrecognized token {:?} at line {}, column {}",
                        token, line, col
                    ))
                    .with_label(
                        Label::new((filepath.clone(), char_start..char_end))
                            .with_message({
                                if !expected.is_empty() {
                                    let expected_str = expected.join(", ");
                                    format!("Expected one of: {}", expected_str)
                                } else {
                                    "Invalid token".to_string()
                                }
                            })
                            .with_color(Color::Red),
                    )
                    .with_label(
                        Label::new((filepath, span_start_char..span_end_char))
                            .with_message("The error span including all tokens")
                            .with_color(Color::Cyan),
                    )
            }
            UnrecognizedEof { location, expected } => {
                let (line, col) = byte_offset_to_position(source, *location);
                let char_offset = byte_offset_to_char_offset(source, *location);
                let (span_start_char, span_end_char) = match span {
                    Some(span) => (
                        byte_offset_to_char_offset(source, span.0),
                        byte_offset_to_char_offset(source, span.1),
                    ),
                    None => (char_offset, char_offset + 1),
                };
                Report::build(ReportKind::Error, filepath.clone(), char_offset)
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
            ExtraToken { token: (start, token, end) } => {
                let (line, col) = byte_offset_to_position(source, *start);
                let char_start = byte_offset_to_char_offset(source, *start);
                let char_end = byte_offset_to_char_offset(source, *end);
                let (span_start_char, span_end_char) = match span {
                    Some(span) => (
                        byte_offset_to_char_offset(source, span.0),
                        byte_offset_to_char_offset(source, span.1),
                    ),
                    None => (char_start, char_end),
                };
                Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message(format!(
                        "Extra token {:?} at line {}, column {}",
                        token, line, col
                    ))
                    .with_label(
                        Label::new((filepath.clone(), char_start..char_end))
                            .with_message("Try removing this token")
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new((filepath, span_start_char..span_end_char))
                            .with_message("The error span including all tokens")
                            .with_color(Color::Cyan),
                    )
            }
            User { error: lex_error } => {
                let (line, col) = byte_offset_to_position(source, lex_error.span().start);
                let char_start = byte_offset_to_char_offset(source, lex_error.span().start);
                let char_end = byte_offset_to_char_offset(source, lex_error.span().end);
                let (span_start_char, span_end_char) = match span {
                    Some(span) => (
                        byte_offset_to_char_offset(source, span.0),
                        byte_offset_to_char_offset(source, span.1),
                    ),
                    None => (char_start, char_end),
                };
                Report::build(ReportKind::Error, filepath.clone(), char_start)
                    .with_message(format!(
                        "Lexical error at line {}, column {}: {}",
                        line, col, lex_error
                    ))
                    .with_label(
                        Label::new((filepath.clone(), char_start..char_end))
                            .with_message("There is a lexical error here")
                            .with_color(Color::Red),
                    )
                    .with_label(
                        Label::new((filepath, span_start_char..span_end_char))
                            .with_message("The error span including all tokens")
                            .with_color(Color::Cyan),
                    )
            }
        }
        .finish()
    }
}

pub fn inject_std_library(
    ast: WithLocation<BasicTypeAst>,
    errors: &mut Vec<WithLocation<MultiFileBuilderError>>,
) -> WithLocation<BasicTypeAst> {
    let std_lib_code = r##"
    let $"op#true": any = True::();
    let $"op#false": any = False::();
    let $"op#and": any = match
        | ($"op#true", $"op#true") => $"op#true"
        | ($"op#true", $"op#false") => $"op#false"
        | ($"op#false", $"op#true") => $"op#false"
        | ($"op#false", $"op#false") => $"op#false"
        | panic;
    let $"op#or": any = match
        | ($"op#true", $"op#true") => $"op#true"
        | ($"op#true", $"op#false") => $"op#true"
        | ($"op#false", $"op#true") => $"op#true"
        | ($"op#false", $"op#false") => $"op#false"
        | panic;
    let $"op#not": any = match
        | $"op#true" => $"op#false"
        | $"op#false" => $"op#true"
        | panic;
    let $"op#add": any = (x: any, y: any) => __add!(x, y);
    let $"op#sub": any = (x: any, y: any) => __sub!(x, y);
    let $"op#mul": any = (x: any, y: any) => __mul!(x, y);
    let $"op#div": any = (x: any, y: any) => __div!(x, y);
    let $"op#mod": any = (x: any, y: any) => __mod!(x, y);
    let $"op#gt": any = (x: any, y: any) => __greater!(x, y, true, false);
    let $"op#lt": any = (x: any, y: any) => __less!(x, y, true, false);
    let $"op#eq": any = match
        | (_x: sub _y, _y: sub _x) => true
        | any => false
        | panic;
    let $"op#neq": any = match
        | (_x: sub _y, _y: sub _x) => false
        | any => true
        | panic;
    let $"op#gte": any = match 
        | (_x: sub _y, _y: sub _x) => true
        | (x: any, y: any) => __greater!(x, y, true, false)
        | panic;
    let $"op#lte": any = match
        | (_x: sub _y, _y: sub _x) => true
        | (x: any, y: any) => __less!(x, y, true, false)
        | panic;
    let $"op#neg": any = (x: any) => __neg!(x);
    let $"op#is": any = (x: any, y: any) => __is!(x, y, true, false);
    let $"op#assign": any = (x: any, y: any) => __assign!(x, y);
    $"<placeholder>"
    "##;
    let mut import_ast = HashMap::new();
    let mut path = FastCycleDetector::new();
    let mut builder = MultiFileBuilder::new(&mut import_ast, &mut path, errors);
    let (std_ast_opt, _std_source) =
        builder.build(PathBuf::from("<std>"), std_lib_code.to_string());
    // 合并 AST, 把 Variable("<placeholder>") 替换为实际的 ast
    if let Some((std_ast, _)) = std_ast_opt {
        #[stacksafe::stacksafe]
        fn replace_placeholder(
            std_ast: WithLocation<BasicTypeAst>,
            ast: &WithLocation<BasicTypeAst>,
        ) -> WithLocation<BasicTypeAst> {
            let loc = std_ast.location().cloned();
            match std_ast.value {
                BasicTypeAst::Variable(name) if name.value() == "<placeholder>" => ast.clone(),
                BasicTypeAst::Variable(_) => std_ast,
                BasicTypeAst::Range { ty, min, delta } => WithLocation::new(
                    BasicTypeAst::Range { ty: replace_placeholder(*ty, ast).into(), min, delta },
                    loc.as_ref(),
                ),
                BasicTypeAst::NaturalNumberSet => std_ast,
                BasicTypeAst::Float => std_ast,
                BasicTypeAst::Char => std_ast,
                BasicTypeAst::NaturalNumberLiteral(_) => std_ast,
                BasicTypeAst::FloatLiteral(_) => std_ast,
                BasicTypeAst::CharLiteral(_) => std_ast,
                BasicTypeAst::Tuple(items) => WithLocation::new(
                    BasicTypeAst::Tuple(
                        items.into_iter().map(|(s, n)| (replace_placeholder(s, ast), n)).collect(),
                    ),
                    loc.as_ref(),
                ),
                BasicTypeAst::List { head, tail } => WithLocation::new(
                    BasicTypeAst::List {
                        head: head
                            .into_iter()
                            .map(|(s, n)| (replace_placeholder(s, ast), n))
                            .collect(),
                        tail: replace_placeholder(*tail, ast).into(),
                    },
                    loc.as_ref(),
                ),
                BasicTypeAst::Cons { head, tail } => WithLocation::new(
                    BasicTypeAst::Cons {
                        head: head
                            .into_iter()
                            .map(|(s, n)| (replace_placeholder(s, ast), n))
                            .collect(),
                        tail: replace_placeholder(*tail, ast).into(),
                    },
                    loc.as_ref(),
                ),
                BasicTypeAst::AnyOf(items) => WithLocation::new(
                    BasicTypeAst::AnyOf(
                        items.into_iter().map(|item| replace_placeholder(item, ast)).collect(),
                    ),
                    loc.as_ref(),
                ),
                BasicTypeAst::AllOf(items) => WithLocation::new(
                    BasicTypeAst::AllOf(
                        items.into_iter().map(|item| replace_placeholder(item, ast)).collect(),
                    ),
                    loc.as_ref(),
                ),
                BasicTypeAst::Invoke { func, arg, continuation, perform_handler } => {
                    WithLocation::new(
                        BasicTypeAst::Invoke {
                            func: Box::new(replace_placeholder(*func, ast)),
                            arg: Box::new(replace_placeholder(*arg, ast)),
                            continuation: continuation
                                .map(|c| Box::new(replace_placeholder(*c, ast))),
                            perform_handler: perform_handler
                                .map(|h| Box::new(replace_placeholder(*h, ast))),
                        },
                        loc.as_ref(),
                    )
                }
                BasicTypeAst::Match { branches } => WithLocation::new(
                    BasicTypeAst::Match {
                        branches: branches
                            .into_iter()
                            .map(|(pattern_variant, expr)| {
                                let new_expr = replace_placeholder(expr, ast);
                                let new_variant = match pattern_variant {
                                    BasicGenericPattern::Standard { pattern, constraint } => {
                                        BasicGenericPattern::Standard {
                                            pattern: replace_placeholder(pattern, ast),
                                            constraint: constraint
                                                .into_iter()
                                                .map(|(v, expr)| {
                                                    (v, replace_placeholder(expr, ast))
                                                })
                                                .collect(),
                                        }
                                    }
                                    BasicGenericPattern::AutoBind { pattern } => {
                                        BasicGenericPattern::AutoBind {
                                            pattern: replace_placeholder(pattern, ast),
                                        }
                                    }
                                };
                                (new_variant, new_expr)
                            })
                            .collect(),
                    },
                    loc.as_ref(),
                ),
                BasicTypeAst::Lambda { patterns } => WithLocation::new(
                    BasicTypeAst::Lambda {
                        patterns: patterns
                            .into_iter()
                            .map(|pattern_variant| match pattern_variant {
                                BasicGenericPattern::Standard { pattern, constraint } => {
                                    BasicGenericPattern::Standard {
                                        pattern: replace_placeholder(pattern, ast),
                                        constraint: constraint
                                            .into_iter()
                                            .map(|(v, expr)| (v, replace_placeholder(expr, ast)))
                                            .collect(),
                                    }
                                }
                                BasicGenericPattern::AutoBind { pattern } => {
                                    BasicGenericPattern::AutoBind {
                                        pattern: replace_placeholder(pattern, ast),
                                    }
                                }
                            })
                            .collect(),
                    },
                    loc.as_ref(),
                ),
                BasicTypeAst::Bind { var, expr } => WithLocation::new(
                    BasicTypeAst::Bind { var, expr: replace_placeholder(*expr, ast).into() },
                    loc.as_ref(),
                ),
                BasicTypeAst::Apply { func, arg, handler, auto_cps } => WithLocation::new(
                    BasicTypeAst::Apply {
                        func: replace_placeholder(*func, ast).into(),
                        arg: replace_placeholder(*arg, ast).into(),
                        handler: handler.map(|h| replace_placeholder(*h, ast).into()),
                        auto_cps,
                    },
                    loc.as_ref(),
                ),
                BasicTypeAst::AtomicOpcode(_) => std_ast,
                BasicTypeAst::Namespace { tag, expr } => WithLocation::new(
                    BasicTypeAst::Namespace { tag, expr: replace_placeholder(*expr, ast).into() },
                    loc.as_ref(),
                ),
                BasicTypeAst::Generic(pattern) => WithLocation::new(
                    BasicTypeAst::Generic(
                        (match *pattern {
                            BasicGenericPattern::Standard { pattern, constraint } => {
                                BasicGenericPattern::Standard {
                                    pattern: replace_placeholder(pattern, ast),
                                    constraint: constraint
                                        .into_iter()
                                        .map(|(v, expr)| (v, replace_placeholder(expr, ast)))
                                        .collect(),
                                }
                            }
                            BasicGenericPattern::AutoBind { pattern } => {
                                BasicGenericPattern::AutoBind {
                                    pattern: replace_placeholder(pattern, ast),
                                }
                            }
                        })
                        .into(),
                    ),
                    loc.as_ref(),
                ),
                BasicTypeAst::Lazy(v) => WithLocation::new(
                    BasicTypeAst::Lazy(replace_placeholder(*v, ast).into()),
                    loc.as_ref(),
                ),
                BasicTypeAst::Mutable { value } => WithLocation::new(
                    BasicTypeAst::Mutable { value: replace_placeholder(*value, ast).into() },
                    loc.as_ref(),
                ),
                BasicTypeAst::SubOf { value } => WithLocation::new(
                    BasicTypeAst::SubOf { value: replace_placeholder(*value, ast).into() },
                    loc.as_ref(),
                ),
                BasicTypeAst::StaticFixPoint { param_name, expr } => WithLocation::new(
                    BasicTypeAst::StaticFixPoint {
                        param_name,
                        expr: replace_placeholder(*expr, ast).into(),
                    },
                    loc.as_ref(),
                ),
            }
        }
        replace_placeholder(std_ast, &ast)
    } else {
        ast
    }
}
