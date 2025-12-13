let List where List: any = T where T: any => rec list: (() | (T ~ list));
let print_chars where print_chars: any = rec print_chars: str where str: List(char) =>
    match str
        | () where {} => ()
        | (head ~ tail) where { head: char, tail: any} => (discard print!(head); print_chars(tail))
        | panic;
print_chars("Hello, world!\nThis is a simple string printing example in Mutica.")