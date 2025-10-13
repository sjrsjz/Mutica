let list_pkg: any = import "list.mu";
let maybe_pkg: any = import "maybe.mu";

let String: any = list_pkg.List(char);

let println: any = s: String |-> {
    discard list_pkg.iter(s)(c: char |-> {
        discard print!(c);
    });
    discard print!('\n');
};

let print: any = s: String |-> {
    discard list_pkg.iter(s)(c: char |-> {
        discard print!(c);
    });
};

let slice: any = (s: String, start: int, end: int) |-> {
    let len: int = list_pkg.len(s);
    match (start >= 0 & start <= len & end >= start & end <= len)
        | false => maybe_pkg.Nothing
        | true => maybe_pkg.Just(list_pkg.take(list_pkg.drop(s)(start))(end - start))
        | panic
};

String::String &
println::println &
print::print &
slice::slice