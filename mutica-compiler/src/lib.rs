use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub grammar, "/parser/grammar.rs");

pub mod parser;
pub mod util;
pub use ariadne;
pub use logos;
pub use lalrpop_util;

use ariadne::{Color, Label, Report, ReportKind};

use crate::{
    parser::lexer::{LexerToken, LexicalError},
    util::byte_offset_to_char_offset,
};

pub struct SyntaxError<'input>(lalrpop_util::ParseError<usize, LexerToken<'input>, LexicalError>);
impl<'input> SyntaxError<'input> {
    pub fn new(e: lalrpop_util::ParseError<usize, LexerToken<'input>, LexicalError>) -> Self {
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
