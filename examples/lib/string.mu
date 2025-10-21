let list_pkg: any = import "list.mu";
let maybe_pkg: any = import "maybe.mu";

let String: any = list_pkg.List(char);

let println: any = s: String -> {
    discard list_pkg.iter(s)(c: char -> {
        discard print!(c);
    });
    discard print!('\n');
};

let print: any = s: String -> {
    discard list_pkg.iter(s)(c: char -> {
        discard print!(c);
    });
};

let slice: any = (s: String, start: int, end: int) -> {
    let len: int = list_pkg.len(s);
    if (start >= 0 & start <= len & end >= start & end <= len)
        then maybe_pkg.Just(list_pkg.take(list_pkg.drop(s)(start))(end - start))
        else maybe_pkg.Nothing
};

String::String &
println::println &
print::print &
slice::slice