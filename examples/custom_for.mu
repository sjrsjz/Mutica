let constraint {
    String::(String: any) &
    println::(println: lambda) &
    nat_to_string::(nat_to_string: lambda)
} = import "lib/string.mu";
let constraint {
    iter::(iter: lambda)
} = import "lib/list.mu";
let constraint {
    while::(while: lambda)
} = import "lib/controlflow.mu";
let constraint {
    Just::(Just: lambda) &
    Nothing::(Nothing: any)
} = import "lib/maybe.mu";

discard iter constraint v: String = (
    "This is string 1",
    "This is string 2",
    "This is string 3"
) in {
    discard println v;
};

discard while constraint n: nat = 0 in {
    match n < 5
        | assert true => {
            discard println( "While loop iteration: " + nat_to_string(n) );
            Just(n + 1)
        }
        | assert false => Nothing
        | panic
};