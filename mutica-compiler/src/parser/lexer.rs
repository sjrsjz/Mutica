use logos::{Lexer, Logos};
use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
    ops::Range,
};

pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidToken(Range<usize>),
    InvalidInteger(ParseIntError, Range<usize>),
    InvalidFloat(ParseFloatError, Range<usize>),
    InvalidRepeatCount(usize, usize, usize),
}

impl Default for LexicalError {
    fn default() -> Self {
        LexicalError::InvalidToken(0..0)
    }
}

impl LexicalError {
    pub fn span(&self) -> Range<usize> {
        match self {
            LexicalError::InvalidToken(span) => span.clone(),
            LexicalError::InvalidInteger(_, span) => span.clone(),
            LexicalError::InvalidFloat(_, span) => span.clone(),
            LexicalError::InvalidRepeatCount(_, start, end) => *start..*end,
        }
    }
}

impl Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexicalError::InvalidToken(_) => write!(f, "Invalid token"),
            LexicalError::InvalidInteger(e, _) => write!(f, "Invalid integer: {}", e),
            LexicalError::InvalidFloat(e, _) => write!(f, "Invalid float: {}", e),
            LexicalError::InvalidRepeatCount(n, _, _) => {
                write!(f, "Invalid repeat count: {} (must be greater than 0)", n)
            }
        }
    }
}

// This function is the callback for the CharLit token.
// It receives the lexer and returns an Option<char>.
// If parsing fails, returning None will cause logos to fall back and try other rules or produce an error.
fn parse_char_literal(lex: &mut Lexer<LexerToken>) -> Option<char> {
    // Get the matched slice, e.g., "'a'", "'\\n'", "'\\u{4E2D}'"
    let slice = lex.slice();

    // Remove the surrounding single quotes
    let inner = &slice[1..slice.len() - 1];

    // Check if it's an escape sequence
    if inner.starts_with('\\') {
        // It's an escape sequence, get the character after '\'
        let mut chars = inner.chars();
        chars.next(); // Consume the backslash

        match chars.next()? {
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            '\\' => Some('\\'),
            '\'' => Some('\''),
            '"' => Some('"'),
            'u' => {
                // It's a Unicode escape \u{...}
                // The regex already validated the format, so we can be confident here
                let hex_code = &inner[3..inner.len() - 1]; // Get the content inside {}
                let char_code = u32::from_str_radix(hex_code, 16).ok()?;
                std::char::from_u32(char_code)
            }
            _ => None, // Should not happen if regex is correct
        }
    } else {
        // It's a normal character like 'a' or '中'
        // Just return the first (and only) char
        inner.chars().next()
    }
}

// 解析整型字面量 (支持 0x, 0o, 0b)
fn parse_nature_number(lex: &mut Lexer<LexerToken>) -> Result<usize, LexicalError> {
    let slice = lex.slice();
    if slice.starts_with("0x") || slice.starts_with("0X") {
        usize::from_str_radix(&slice[2..], 16)
            .map_err(|e| LexicalError::InvalidInteger(e, lex.span()))
    } else if slice.starts_with("0o") || slice.starts_with("0O") {
        usize::from_str_radix(&slice[2..], 8)
            .map_err(|e| LexicalError::InvalidInteger(e, lex.span()))
    } else if slice.starts_with("0b") || slice.starts_with("0B") {
        usize::from_str_radix(&slice[2..], 2)
            .map_err(|e| LexicalError::InvalidInteger(e, lex.span()))
    } else {
        slice.parse().map_err(|e| LexicalError::InvalidInteger(e, lex.span()))
    }
}

// 解析浮点数字面量
fn parse_float(lex: &mut Lexer<LexerToken>) -> Result<f64, LexicalError> {
    lex.slice().parse().map_err(|e| LexicalError::InvalidFloat(e, lex.span()))
}

// 解析字符串字面量，处理转义序列
fn parse_string_literal(lex: &mut Lexer<LexerToken>) -> Result<String, LexicalError> {
    let slice = lex.slice();
    // 去掉前后的双引号
    let inner = &slice[1..slice.len() - 1];
    let mut result = String::new();
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next().ok_or(LexicalError::InvalidToken(lex.span()))? {
                'n' => result.push('\n'),
                'r' => result.push('\r'),
                't' => result.push('\t'),
                '\\' => result.push('\\'),
                '\'' => result.push('\''),
                '"' => result.push('"'),
                'u' => {
                    // 简化版 Unicode 处理，可以后续增强
                    // 跳过 '{'
                    chars.next();
                    let hex_code: String = chars.by_ref().take_while(|&c| c != '}').collect();
                    let char_code = u32::from_str_radix(&hex_code, 16)
                        .map_err(|_| LexicalError::InvalidToken(lex.span()))?;
                    result.push(
                        std::char::from_u32(char_code)
                            .ok_or(LexicalError::InvalidToken(lex.span()))?,
                    );
                }
                _ => return Err(LexicalError::InvalidToken(lex.span())),
            }
        } else {
            result.push(c);
        }
    }
    Ok(result)
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"\s+")]
#[logos(skip r"//[^\n\r]*")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
#[logos(error = LexicalError)]
pub enum LexerToken {
    // 浮点数：小数点后必须有至少一个数字，避免与 Range (..) 冲突
    // 匹配: 1.5, 1.5e10, 1e10
    // 不匹配: 1., .5 (这些会与 Range 冲突)
    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+", parse_float)]
    FloatNum(f64),
    // 支持十进制、十六进制(0x)、八进制(0o)、二进制(0b)
    #[regex("0[xX][0-9a-fA-F]+|0[oO][0-7]+|0[bB][01]+|[0-9]+", parse_nature_number)]
    Num(usize),
    #[token("_", priority = 3)]
    Wildcard,
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Ident(String),
    #[regex(r#"'(\\u\{[0-9a-fA-F]{1,6}\}|\\[nrt\\'"]|[^'\\])'"#, parse_char_literal)]
    CharLit(char),

    #[regex(r#""([^"\\]|\\.)*""#, parse_string_literal)]
    StringLit(String),

    #[token("let")]
    Let,
    #[token("with")]
    With,
    #[token("where")]
    Where,
    #[token("exist")]
    Exist,
    #[token("assert")]
    Assert,
    #[token("match")]
    Match,
    #[token("rec")]
    Rec,
    #[token("dyn_rec")]
    DynamicRec,
    #[token("loop")]
    Loop,
    #[token("panic")]
    Panic,
    #[token("discard")]
    Discard,
    #[token("nat")]
    Nat,
    #[token("float")]
    Float,
    #[token("char")]
    Char,
    #[token("lambda")]
    Lambda,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("any")]
    Any,
    #[token("unknown")]
    Unknown,
    #[token("never")]
    Never,
    #[token("import")]
    Import,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("rot")]
    Rot,
    #[token("handle")]
    Handle,
    #[token("sub")]
    SubOf,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("extend")]
    Extend,
    #[token("as")]
    As,
    #[token("constraint")]
    Constraint,
    #[token("lazy")]
    Lazy,

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
    #[token("||")]
    DoublePipe,
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
    #[token("&&")]
    DoubleAmpersand,
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
    #[token("is")]
    Is,
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
    #[token("..", priority = 3)]
    Range,
    #[token("$")]
    Dollar,
}
