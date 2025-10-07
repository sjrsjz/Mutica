let print_chars: any = rec print_chars: (chars: (() | (char, any))) |->
    match chars
        | () => ()
        | (head: char, tail: any) => (discard print(head); print_chars(tail))
        | panic;
print_chars("Hello, world!\n")