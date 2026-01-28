let List: (lambda | T: any | panic) = T: any => rec list: (() | (T ~ list));
let print_chars: (lambda | str: List(char) | panic) = dyn_rec print_chars: str: List(char) =>
    match str
        | () => ()
        | (head: char ~ tail: any) => (discard print!(head); print_chars(tail))
        | panic;
print_chars("Hello, world!\nThis is a simple string printing example in Mutica.")