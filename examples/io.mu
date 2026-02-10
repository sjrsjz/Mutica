let List: sub (_T: any => unknown) = T: any => rec list: (() | (T ~ list));
let print_chars: sub (_str: List(char) => unknown) = dyn_rec print_chars: str: List(char) =>
    match str
        | () => ()
        | (head: char ~ tail: any) => (print!(head); print_chars(tail))
        | panic;
print_chars("Hello, world!\nThis is a simple string printing example in Mutica.")