let constraint list_pkg: any = import "list.mu";
let constraint maybe_pkg: any = import "maybe.mu";
let constraint {
    List::(List: any) &
    iter::(iter: any) &
    len::(len: any) &
    take::(take: any) &
    drop::(drop: any)
} = list_pkg;
let constraint Just::(Just: any) = maybe_pkg;
let constraint Nothing::(Nothing: any) = maybe_pkg;

let constraint String: any = List(char);

let constraint println: any = constraint s: String => {
    discard iter constraint c: char = s in {
        discard print!(c);
    };
    discard print!('\n');
};

let constraint print: any = constraint s: String => {
    iter constraint c: char = s in {
        discard print!(c);
    }
};

let constraint slice: any = constraint (s: String, start: nat, end: nat) => {
    let constraint len: nat = len(s);
    if (start >= 0 && start <= len && end >= start && end <= len)
        then Just(take(drop(s)(start))(end - start))
        else Nothing
};

let constraint nat_to_string: any = 
    match
        | assert 0 => "0"
        | constraint n: nat => {
            loop go: constraint (acc: String, n: nat) = ((), n);
                if n == 0 then acc
                else {
                    let constraint digit: (char,) = match n % 10
                        | assert 0 => "0"
                        | assert 1 => "1"
                        | assert 2 => "2"
                        | assert 3 => "3"
                        | assert 4 => "4"
                        | assert 5 => "5"
                        | assert 6 => "6"
                        | assert 7 => "7"
                        | assert 8 => "8"
                        | assert 9 => "9"
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