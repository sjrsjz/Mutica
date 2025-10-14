use logos::Logos;
use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct LexicalError {
    pub span: Span,
}

impl Default for LexicalError {
    fn default() -> Self {
        LexicalError { span: 0..0 }
    }
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"\s+")]
#[logos(skip r"//[^\n\r]*")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
#[logos(error = LexicalError)]
pub enum LexerToken {
    // 支持十进制、十六进制(0x)、八进制(0o)、二进制(0b)
    #[regex("0[xX][0-9a-fA-F]+|0[oO][0-7]+|0[bB][01]+|[0-9]+", |lex| lex.slice().to_owned())]
    Num(String),
    #[token("_", priority = 3)]
    Wildcard,
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Ident(String),

    #[regex(r"'(\\'|\x22|\\n|\\r|\\t|\\\\|[^'\\])'", |lex| lex.slice().to_owned())]
    CharLit(String),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_owned())]
    StringLit(String),

    #[token("let")]
    Let,
    #[token("perform")]
    Perform,
    #[token("with")]
    With,
    #[token("match")]
    Match,
    #[token("rec")]
    Rec,
    #[token("panic")]
    Panic,
    #[token("discard")]
    Discard,
    #[token("int")]
    Int,
    #[token("char")]
    Char,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("any")]
    Any,
    #[token("import")]
    Import,
    #[token("none")]
    None,
    #[token("__add")]
    DunderAdd,
    #[token("__sub")]
    DunderSub,
    #[token("__mul")]
    DunderMul,
    #[token("__div")]
    DunderDiv,
    #[token("__mod")]
    DunderMod,
    #[token("__is")]
    DunderIs,
    #[token("__opcode")]
    DunderOpcode,

    #[token("->")]
    Arrow,
    #[token("|->")]
    FatArrow,
    #[token("=>")]
    DoubleArrow,
    #[token("::")]
    DoubleColon,
    #[token(".")]
    Dot,
    #[token("@")]
    At,
    #[token("|")]
    Pipe,
    #[token("!")]
    Bang,
    #[token(":")]
    Colon,
    #[token("~")]
    Tilde,
    #[token(",")]
    Comma,
    #[token("&")]
    Ampersand,
    #[token("==")]
    Eq,
    #[token("!=")]
    Neq,
    #[token("<")]
    Lt,
    #[token("<=")]
    Lte,
    #[token(">")]
    Gt,
    #[token(">=")]
    Gte,
    #[token("<:")]
    Subtype,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("=")]
    Assign,
    #[token(";")]
    Semicolon,
    #[token("#")]
    Hash,
    #[token("\\")]
    Backslash,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("|>")]
    PipeGreaterThan,
}
