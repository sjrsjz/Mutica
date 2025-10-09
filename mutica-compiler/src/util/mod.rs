/// 将字节偏移转换为字符偏移 (用于 ariadne)
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
