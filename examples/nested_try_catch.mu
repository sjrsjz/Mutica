let {
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = import "lib/string.mu";

// --- Multi-level Try-Catch using Effect Handlers ---

// Example 1: Basic nested try-catch with re-throw
println("=== Example 1: Basic Nested Try-Catch ===");

println {
    handle with _k1: any => match
        | throw::(err: any) => {
            println("Outer handler caught: " + err);
            "outer_fallback"
        }
        | panic;
    handle with _k2: any => match
        | throw::(err: any) => {
            println("Inner handler caught: " + err);
            // Re-throw to outer handler
            perform! throw::("Re-thrown from inner: " + err)
        }
        | panic;
    println("Before inner throw");
    perform! throw::("ValueError::Invalid input");
    println("should_not_reach");
    ()
};

println("");


// Example 2: Selective handling at different levels
println("=== Example 2: Selective Error Handling ===");

println {
    handle with _k1: any => match
        | throw::(err: any) => {
            println("Outer: Caught - " + err);
            "recovered"
        }
        | panic;
    handle with dyn_rec inner: k2: any => match
        | divide_error::(err: any) => {
            println("Inner: Caught division error - " + err);
            handle with inner;
            k2 () // Return safe default
        }
        | panic;
    println("Starting computation...");
    perform! divide_error::("Cannot divide by zero");
    println("After error handling");
    ()
};

println("");


// Example 3: Try-catch with successful execution
println("=== Example 3: Success Path ===");

println! {
    handle with _k: any => match
        | throw::(err: any) => {
            println("Error: " + err);
            0
        }
        | return::(v: any) => {
            println("Success: returning " + display! v);
            v
        }
        | panic;
    let a: nat = 10;
    let b: nat = 20;
    let sum: nat = a + b;
    println("Computed sum: " + nat_to_string(sum));
    let result: nat = perform! return::(sum);
    println("Final result: " + nat_to_string(result));
    ()
};

println("");


// Example 4: Three-level nested handling
println("=== Example 4: Three Levels of Handling ===");

println {
    handle with _k3: any => match
        | throw::(err: any) => {
            println("Level 3 (final): Caught - " + err);
            "level3_handled"
        }
        | panic;
    handle with _k2: any => match
        | throw::(err: any) => {
            println("Level 2: Caught - " + err);
            perform! throw::("Level2 -> " + err)
        }
        | panic;
    handle with _k1: any => match
        | throw::(err: any) => {
            println("Level 1: Caught - " + err);
            perform! throw::("Level1 -> " + err)
        }
        | panic;
    println("Starting computation...");
    perform! throw::("Original error");
    ()
};

println("");


// Example 5: Practical example with division and validation
println("=== Example 5: Practical Operations ===");

{
    handle with _k: any => match
        | div_error::() => {
            println("Application: Handling division error");
            1
        }
        | val_error::() => {
            println("Application: Handling validation error");
            50
        }
        | panic;
    handle with _k: any => match
        | val_error::() => {
            println("Validator: Caught validation error");
            perform! val_error::()
        }
        | panic;
    handle with _k: any => match
        | div_error::() => {
            println("Calculator: Caught division error");
            perform! div_error::()
        }
        | panic;
    println("Starting operations...");
    let x: nat = 20;
    println("After division: " + nat_to_string(x));
    if x > 100
        then perform! val_error::()
        else println("Validation passed");
    println("Operations completed successfully");
    ()
};

println("");


// Example 6: With cleanup/finally behavior
println("=== Example 6: Cleanup Behavior ===");

println {
    handle with dyn_rec h: k: any => match
        | throw::(err: any) => {
            println("Error caught: " + err);
            "error_handled"
        }
        | cleanup::() => {
            println("Cleanup executed");
            handle with h;
            k ()
        }
        | panic;
    println("Starting operation...");
    perform! cleanup::();
    println("After cleanup");
    perform! throw::("An error occurred");
    println("This will not be reached");
    ()
};

println("");
println("=== All examples completed ===");
