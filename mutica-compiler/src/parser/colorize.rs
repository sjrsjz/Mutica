#[derive(Clone, Copy)]
pub enum TokenColor {
    UnSpecified,
    Keyword,
    Identifier,
    Declaration,
    Namespace,
    Literal,
    Operator,
    Comment,
    Whitespace,
    Punctuation,
    Function,
    Type,
    Attribute,
    Macro,
    Number,
    String,
    Boolean,
    Error,
}
impl TokenColor {
    pub fn fill_colors(
        colors: &mut [TokenColor],
        range: std::ops::Range<usize>,
        color: TokenColor,
    ) {
        for i in range {
            if i < colors.len() {
                colors[i] = color;
            }
        }
    }

    pub fn new_buffer(len: usize) -> Vec<TokenColor> {
        vec![TokenColor::UnSpecified; len]
    }
}
