let {
    String::(String: any) &
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = import "lib/string.mu";
let {
    iter::(iter: any)
} = import "lib/list.mu";
let {
    while::(while: any)
} = import "lib/controlflow.mu";
let {
    Just::(Just: any) &
    Nothing::(Nothing: any)
} = import "lib/maybe.mu";

discard @iter v: String = (
    "This is string 1",
    "This is string 2",
    "This is string 3"
) in {
    discard println v;
};

discard @while n: nat = 0 in {
    match n < 5
        | true => {
            discard println( "While loop iteration: " + nat_to_string(n) );
            Just(n + 1)
        }
        | false => Nothing
        | panic
};