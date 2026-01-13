let list_pkg: any = import "list.mu";
let maybe_pkg: any = import "maybe.mu";
let {
    List::(List: lambda) &
    iter::(iter: lambda) &
    len::(len: lambda) &
    take::(take: lambda) &
    drop::(drop: lambda)
} = list_pkg;
let Just::(Just: lambda) = maybe_pkg;
let Nothing::(Nothing: any) = maybe_pkg;

let String: any = List(char);

let println: lambda = s: String => {
    discard @iter c: char = s in {
        discard print!(c);
    };
    discard print!('\n');
};

let print: lambda = s: String => {
    @iter c: char = s in {
        discard print!(c);
    }
};

let slice: lambda = (s: String, start: nat, end: nat) => {
    let len: nat = len(s);
    if (start >= 0 && start <= len && end >= start && end <= len)
        then Just(take(drop(s)(start))(end - start))
        else Nothing
};

let nat_to_string: lambda = 
    match
        | 0 => "0"
        | n: nat => {
            loop go: (acc: String, n: nat) = ((), n);
                if n == 0 then acc
                else {
                    let digit: (char,) = match n % 10
                        | 0 => "0"
                        | 1 => "1"
                        | 2 => "2"
                        | 3 => "3"
                        | 4 => "4"
                        | 5 => "5"
                        | 6 => "6"
                        | 7 => "7"
                        | 8 => "8"
                        | 9 => "9"
                        | panic;
                    go((digit + acc, n / 10))
                }
        }
        | panic;

String::String &
println::println &
print::print &
slice::slice &
nat_to_string::nat_to_string