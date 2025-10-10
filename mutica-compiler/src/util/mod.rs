pub fn byte_offset_to_char_offset(source: &str, byte_offset: usize) -> usize {
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

/// Convert byte offset to line and column numbers
/// Correctly handles UTF-8 multi-byte characters and different line endings
pub fn byte_offset_to_position(source: &str, byte_offset: usize) -> (usize, usize) {
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
