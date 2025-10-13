let List: any = (T: any) |-> rec list: (() | (T, list));
let print_chars: any = rec print_chars: str: List(char) |->
    match str
        | () => ()
        | (head: char, tail: any) => (discard print!(head); print_chars(tail))
        | panic;
print_chars("Hello, world!\n")