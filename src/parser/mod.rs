pub mod ast;
pub mod lexer;
pub use ast::TypeAst;

use std::{collections::HashMap, fmt::Debug};

use crate::{
    parser::{
        ast::LinearTypeAst,
        lexer::{LexerToken, LexicalError},
    },
    types::Type,
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use lalrpop_util::ErrorRecovery;

/// Convert byte offset to line and column numbers
/// Correctly handles UTF-8 multi-byte characters and different line endings
fn byte_offset_to_position(source: &str, byte_offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;
    let mut current_offset = 0;

    for ch in source.chars() {
        if current_offset >= byte_offset {
            break;
        }

        if ch == '\n' {
            line += 1;
            column = 1;
        } else if ch != '\r' {
            column += 1;
        }

        current_offset += ch.len_utf8();
    }

    (line, column)
}

/// Convert byte offset to character offset
/// This is needed because Ariadne's Span uses character offsets, not byte offsets
fn byte_offset_to_char_offset(source: &str, byte_offset: usize) -> usize {
    let mut char_offset = 0;
    let mut current_byte = 0;

    for ch in source.chars() {
        if current_byte >= byte_offset {
            break;
        }
        current_byte += ch.len_utf8();
        char_offset += 1;
    }

    char_offset
}

/// Calculate the full error span including all dropped tokens
/// Returns (byte_start, byte_end) tuple
fn calculate_full_error_span(
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
pub enum ParseError {
    UseBeforeDeclaration(LinearTypeAst, String),
    RedeclaredPattern(LinearTypeAst, String),
    UnusedVariable(LinearTypeAst, Vec<String>),
    AmbiguousPattern(LinearTypeAst),
    PatternOutOfParameterDefinition(LinearTypeAst),
    MissingBranch(LinearTypeAst),
    InternalError(String),
}

impl ParseError {
    /// 生成一个美观的 ariadne 错误报告
    pub fn report<'a>(&self, filename: &'a str) -> Report<'a, (&'a str, std::ops::Range<usize>)> {
        match self {
            ParseError::UseBeforeDeclaration(_ast, name) => {
                Report::build(ReportKind::Error, filename, 0)
                    .with_message(format!("Use of undeclared variable '{}'", name))
                    .with_label(
                        Label::new((filename, 0..1))
                            .with_message(format!("Variable '{}' is used before declaration", name))
                            .with_color(Color::Red),
                    )
                    .with_help("Make sure the variable is declared before use")
                    .finish()
            }
            ParseError::RedeclaredPattern(_ast, name) => {
                Report::build(ReportKind::Error, filename, 0)
                    .with_message(format!("Redeclared pattern variable '{}'", name))
                    .with_label(
                        Label::new((filename, 0..1))
                            .with_message(format!("Pattern variable '{}' has already been declared", name))
                            .with_color(Color::Red),
                    )
                    .with_help("A pattern cannot contain duplicate variable names")
                    .finish()
            }
            ParseError::UnusedVariable(_ast, names) => {
                let mut report = Report::build(ReportKind::Warning, filename, 0)
                    .with_message(format!("Unused variables: {}", names.join(", ")));
                for name in names {
                    report = report.with_label(
                        Label::new((filename, 0..1))
                            .with_message(format!("Variable '{}' is declared but never used", name))
                            .with_color(Color::Yellow),
                    );
                }
                report
                    .with_help("Consider removing unused variables or prefixing with '_' to intentionally ignore them")
                    .finish()
            }
            ParseError::AmbiguousPattern(_ast) => Report::build(ReportKind::Error, filename, 0)
                .with_message("Ambiguous pattern")
                .with_label(
                    Label::new((filename, 0..1))
                        .with_message("Here: pattern variables are not allowed within generalized/specialized types. Pattern variables may only be used in ordered contexts (e.g., tuples, lists)")
                        .with_color(Color::Red),
                )
                .finish(),
            ParseError::PatternOutOfParameterDefinition(_ast) => {
                Report::build(ReportKind::Error, filename, 0)
                    .with_message("Pattern definition appears in an invalid location")
                    .with_label(
                        Label::new((filename, 0..1))
                            .with_message("Patterns can only be used in parameter definitions")
                            .with_color(Color::Red),
                    )
                    .with_help("Pattern variables may only appear in function parameters or match branch bindings")
                    .finish()
            }
            ParseError::MissingBranch(_ast) => Report::build(ReportKind::Error, filename, 0)
                .with_message("Missing required branch")
                .with_label(
                    Label::new((filename, 0..1))
                        .with_message("A match expression requires at least one branch")
                        .with_color(Color::Red),
                )
                .finish(),
            ParseError::InternalError(msg) => Report::build(ReportKind::Error, filename, 0)
                .with_message("Internal compiler error")
                .with_label(
                    Label::new((filename, 0..1))
                        .with_message(msg.clone())
                        .with_color(Color::Magenta),
                )
                .with_note("This is a compiler bug; please report it to the maintainers")
                .finish(),
        }
    }
}

/// 为 lalrpop 的 ErrorRecovery 生成美观的错误报告
pub fn report_error_recovery<'input>(
    error: &ErrorRecovery<usize, LexerToken<'input>, LexicalError>,
    filename: &str,
    source: &str,
) {
    use lalrpop_util::ParseError::*;

    // Calculate the full error span including dropped tokens
    let (span_start_byte, span_end_byte) = calculate_full_error_span(error);
    let span_start_char = byte_offset_to_char_offset(source, span_start_byte);
    let span_end_char = byte_offset_to_char_offset(source, span_end_byte);

    let mut report = match &error.error {
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
            if !error.dropped_tokens.is_empty() && (span_start_char < char_start || span_end_char > char_end) {
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
            if !error.dropped_tokens.is_empty() && (span_start_char < char_start || span_end_char > char_end) {
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
    };

    // Add note about dropped tokens if any
    if !error.dropped_tokens.is_empty() {
        report = report.with_note(format!(
            "Parser skipped {} token(s) while recovering from this error",
            error.dropped_tokens.len()
        ));
    }

    report.finish().eprint((filename, Source::from(source))).unwrap();
}

pub struct ParseContext {
    pub declared_variables: Vec<HashMap<String, usize>>,
}
pub enum ContextError {
    NotUsed(Vec<String>),
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

    pub fn enter_scope(&mut self) {
        self.declared_variables.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) -> Result<(), ContextError> {
        if let Some(current_scope) = self.declared_variables.last() {
            let unused_vars: Vec<String> = current_scope
                .iter()
                .filter_map(|(name, &count)| {
                    if count == Self::NOT_USED {
                        Some(name.clone())
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

    pub fn declare_variable(&mut self, name: String) -> Result<(), ContextError> {
        if let Some(current_scope) = self.declared_variables.last_mut() {
            if current_scope.contains_key(&name) && current_scope[&name] == Self::NOT_USED {
                return Err(ContextError::NotUsed(vec![name]));
            }
            current_scope.insert(name, Self::NOT_USED);
            return Ok(());
        }
        Err(ContextError::EmptyContext)
    }

    pub fn use_variable(&mut self, name: &str) -> Result<(), ContextError> {
        for scope in self.declared_variables.iter_mut().rev() {
            if let Some(count) = scope.get_mut(name) {
                *count += 1;
                return Ok(());
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
