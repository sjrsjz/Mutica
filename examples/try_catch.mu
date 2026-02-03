let {
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = import "lib/string.mu";

// --- Result Monad (Control Flow / CPS Style) ---
// A Result<T, E> is a function: (Success -> Any, Failure -> Any) -> Any

// Constructors for @ syntax
// @return value; -> return(continuation)(value)
let return: any = _k: any => v: any => 
    (success: any, _failure: any) => success(v);

// @throw error; -> throw(continuation)(error)
let throw: any = _k: any => e: any => 
    (_success: any, failure: any) => failure(e);

// Helper to wrap a raw value into a Result (if not using @ syntax)
let to_return: any = v: any => 
    (success: any, _failure: any) => success(v);

let to_throw: any = e: any => 
    (_success: any, failure: any) => failure(e);

// Combinators
let map: any = res: any => f: any =>
    (success: any, failure: any) =>
        res(v: any => success(f(v)), failure);

let map_err: any = res: any => f: any =>
    (success: any, failure: any) =>
        res(success, e: any => failure(f(e)));

let and_then: any = res: any => f: any =>
    (success: any, failure: any) =>
        res(v: any => f(v)(success, failure), failure);

let unwrap_or: any = res: any => default: any =>
    res(v: any => v, _e: any => default);

// Execution / Extraction
// try_catch takes a handler and a Result (the computation)
let try_catch: any = handler: any => res: any =>
    res(v: any => v, handler);


// --- Examples ---

// 1. Basic usage with @ syntax
let test1: nat = try_catch(e: any => {
    println("Error: " + e);
    0
}) {
    @return 42;
};
println("Test 1 (return): " + nat_to_string(test1));

let test2: nat = try_catch(e: any => {
    println("Caught: " + e);
    0
}) {
    @throw "Something went wrong";
};
println("Test 2 (throw): " + nat_to_string(test2));


// 2. Using map and map_err
// We can define a computation block and then map over it
let computation: any = {
    @return 10;
};

let mapped_computation: any = map(computation)(x: nat => x * 2);

let test3: nat = try_catch(_e: any => 0)(mapped_computation);
println("Test 3 (map): " + nat_to_string(test3));


// 3. Complex flow with and_then (chaining) using Syntax Sugar
// "Do notation" style: and_then var = value in expression
let divide: any = (a: nat, b: nat) =>
    if b == 0
        then to_throw("Division by zero")
        else to_return(a / b);

let complex_calc: any = 
    @and_then x: nat = divide(100, 2) in // Ok(50)
    if x > 20 
        then to_return(x - 20) // Ok(30)
        else to_throw("Result too small");

let test4: nat = try_catch(e: any => {
    println("Calc Error: " + e);
    0
})(complex_calc);

println("Test 4 (sugar): " + nat_to_string(test4));

// 3.1 Multi-step chaining (Do-notation)
let chained_calc: any =
    @and_then a: nat = divide(100, 2) in // 50
    @and_then b: nat = divide(a, 5) in   // 10
    @and_then c: nat = divide(b, 2) in   // 5
    to_return(c + 1); // 6

let test_chain: nat = try_catch(e: any => 0)(chained_calc);
println("Test Chain: " + nat_to_string(test_chain));


// 4. Using map_err to transform errors
let test5: nat = try_catch(e: any => {
    println("Transformed Error: " + e);
    0
})(
    map_err(
        divide(10, 0) // Err("Division by zero")
    )(e: any => "Math Error: " + e)
);
println("Test 5 (map_err): " + nat_to_string(test5));