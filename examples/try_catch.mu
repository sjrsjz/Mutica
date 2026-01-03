let constraint {
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = import "lib/string.mu";

// --- Result Monad (Control Flow / CPS Style) ---
// A Result<T, E> is a function: (Success -> Any, Failure -> Any) -> Any

// Constructors for @ syntax
// @return value; -> return(continuation)(value)
let constraint return: any = constraint _k: lambda => constraint v: any => 
    constraint (success: any, _failure: any) => success(v);

// @throw error; -> throw(continuation)(error)
let constraint throw: any = constraint _k: lambda => constraint e: any => 
    constraint (_success: any, failure: any) => failure(e);

// Helper to wrap a raw value into a Result (if not using @ syntax)
let constraint to_return: any = constraint v: any => 
    constraint (success: any, _failure: any) => success(v);

let constraint to_throw: any = constraint e: any => 
    constraint (_success: any, failure: any) => failure(e);

// Combinators
let constraint map: any = constraint res: lambda => constraint f: lambda =>
    constraint (success: any, failure: any) =>
        res(constraint v: any => success(f(v)), failure);

let constraint map_err: any = constraint res: lambda => constraint f: lambda =>
    constraint (success: any, failure: any) =>
        res(success, constraint e: any => failure(f(e)));

let constraint and_then: any = constraint res: lambda => constraint f: lambda =>
    constraint (success: any, failure: any) =>
        res(constraint v: any => f(v)(success, failure), failure);

let constraint unwrap_or: any = constraint res: lambda => constraint default: any =>
    res(constraint v: any => v, constraint _e: any => default);

// Execution / Extraction
// try_catch takes a handler and a Result (the computation)
let constraint try_catch: any = constraint handler: lambda => constraint res: lambda =>
    res(constraint v: any => v, handler);


// --- Examples ---

// 1. Basic usage with @ syntax
let constraint test1: nat = try_catch(constraint e: any => {
    discard println("Error: " + e);
    0
}) {
    @return 42;
};
discard println("Test 1 (return): " + nat_to_string(test1));

let constraint test2: nat = try_catch(constraint e: any => {
    discard println("Caught: " + e);
    0
}) {
    @throw "Something went wrong";
};
discard println("Test 2 (throw): " + nat_to_string(test2));


// 2. Using map and map_err
// We can define a computation block and then map over it
let constraint computation: any = {
    @return 10;
};

let constraint mapped_computation: any = map(computation)(constraint x: nat => x * 2);

let constraint test3: nat = try_catch(constraint _e: any => 0)(mapped_computation);
discard println("Test 3 (map): " + nat_to_string(test3));


// 3. Complex flow with and_then (chaining) using Syntax Sugar
// "Do notation" style: and_then constraint var = value in expression
let constraint divide: any = constraint (a: nat, b: nat) =>
    if b == 0
        then to_throw("Division by zero")
        else to_return(a / b);

let constraint complex_calc: any = 
    and_then constraint x: nat = divide(100, 2) in // Ok(50)
    if x > 20 
        then to_return(x - 20) // Ok(30)
        else to_throw("Result too small");

let constraint test4: nat = try_catch(constraint e: any => {
    discard println("Calc Error: " + e);
    0
})(complex_calc);

discard println("Test 4 (sugar): " + nat_to_string(test4));

// 3.1 Multi-step chaining (Do-notation)
let constraint chained_calc: any =
    and_then constraint a: nat = divide(100, 2) in // 50
    and_then constraint b: nat = divide(a, 5) in   // 10
    and_then constraint c: nat = divide(b, 2) in   // 5
    to_return(c + 1); // 6

let constraint test_chain: nat = try_catch(constraint e: any => 0)(chained_calc);
discard println("Test Chain: " + nat_to_string(test_chain));


// 4. Using map_err to transform errors
let constraint test5: nat = try_catch(constraint e: any => {
    discard println("Transformed Error: " + e);
    0
})(
    map_err(
        divide(10, 0) // Err("Division by zero")
    )(constraint e: any => "Math Error: " + e)
);
discard println("Test 5 (map_err): " + nat_to_string(test5));