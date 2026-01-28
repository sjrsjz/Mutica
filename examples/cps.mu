let {
    String::(String: any) &
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = import "lib/string.mu";
let Ok: any = T: any => Ok::T;
let Err: any = E: any => Err::E;
let Result: any = (T: any, E: any) => (Ok T | Err E);
let try: any = continuation: any => res: Result(any, any) => 
    match res
        | Ok::(x: any) => continuation(x)
        | Err::(e: any) => Err(e)
        | panic;

let assert_nonzero: any = match
    | 0 => Err("Zero value")
    | n: nat => Ok(n)
    | panic;

let divide: any = a: nat => b: nat => {
    let b: nat = #try assert_nonzero b;
    Ok(a / b)
};

let result1: Result(nat, String) = divide 10 2;
let result2: Result(nat, String) = divide 10 0;
let print_result: any = res: Result(nat, String) => 
    match res
        | Ok::(x: nat) => println("Result: " + nat_to_string x)
        | Err::(e: String) => println("Error: " + e)
        | panic;
discard print_result result1;
discard print_result result2;
