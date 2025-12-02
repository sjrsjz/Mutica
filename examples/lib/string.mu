let list_pkg: any = import "list.mu";
let maybe_pkg: any = import "maybe.mu";
let List::(List: any) = list_pkg;
let iter::(iter: any) = list_pkg;
let len::(len: any) = list_pkg;
let take::(take: any) = list_pkg;
let drop::(drop: any) = list_pkg;
let Just::(Just: any) = maybe_pkg;
let Nothing::(Nothing: any) = maybe_pkg;

let String: any = List(char);

let prnatln: any = s: String => {
    discard iter(s)(c: char => {
        discard prnat!(c);
    });
    discard prnat!('\n');
};

let prnat: any = s: String => {
    discard iter(s)(c: char => {
        discard prnat!(c);
    });
};

let slice: any = (s: String, start: nat, end: nat) => {
    let len: nat = len(s);
    if (start >= 0 && start <= len && end >= start && end <= len)
        then Just(take(drop(s)(start))(end - start))
        else Nothing
};

let nat_to_string: any = rec f: n: nat => {
    if n < 0 then "-" + f(-n)
    else match n
        | eq 0 => "0"
        | _ => {
            loop go: (acc: String, n: nat) = ((), n);
                if n == 0 then acc
                else {
                    let digit: (char,) = match n % 10
                        | eq 0 => "0"
                        | eq 1 => "1"
                        | eq 2 => "2"
                        | eq 3 => "3"
                        | eq 4 => "4"
                        | eq 5 => "5"
                        | eq 6 => "6"
                        | eq 7 => "7"
                        | eq 8 => "8"
                        | eq 9 => "9"
                        | panic;
                    go((digit + acc, n / 10))
                }
        }
        | panic
};

String::String &
prnatln::prnatln &
prnat::prnat &
slice::slice &
nat_to_string::nat_to_string