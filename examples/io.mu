let constraint List: lambda = constraint T: any => rec list: (() | (T ~ list));
let constraint print_chars: lambda = dyn_rec print_chars: constraint str: List(char) =>
    match str
        | assert () => ()
        | constraint (head: char ~ tail: any) => (discard print!(head); print_chars(tail))
        | panic;
print_chars("Hello, world!\nThis is a simple string printing example in Mutica.")