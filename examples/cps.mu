
let Any::(Any: any) = import "lib/any.mu";
let {
    String::(String: any) &
    println::(println: lambda) &
    nat_to_string::(nat_to_string: lambda)
} = import "lib/string.mu";
let Ok: lambda = T: any => Ok::T;
let Err: lambda = E: any => Err::E;
let Result: lambda = (T: any, E: any) => (Ok T | Err E);
let try: lambda = continuation: lambda => res: Result(Any, Any) => 
    match res
        | Ok::(x: any) => continuation(x)
        | Err::(e: any) => Err(e)
        | panic;

let assert_nonzero: lambda = match
    | 0 => Err("Zero value")
    | n: nat => Ok(n)
    | panic;

let divide: lambda = a: nat => b: nat => {
    let b: nat = #try assert_nonzero b;
    Ok(a / b)
};

let result1: Result(nat, String) = divide 10 2;
let result2: Result(nat, String) = divide 10 0;
let print_result: lambda = res: Result(nat, String) => 
    match res
        | Ok::(x: nat) => println("Result: " + nat_to_string x)
        | Err::(e: String) => println("Error: " + e)
        | panic;
discard print_result result1;
discard print_result result2;
