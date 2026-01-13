let {
    println::(println: lambda) &
    nat_to_string::(nat_to_string: lambda)
} = import "lib/string.mu";

// --- Multi-level Try-Catch using Effect Handlers ---

// Example 1: Basic nested try-catch with re-throw
discard println("=== Example 1: Basic Nested Try-Catch ===");

discard println {
    handle with _k1: any => match
        | throw::(err: any) => {
            discard println("Outer handler caught: " + err);
            "outer_fallback"
        }
        | panic;
    handle with _k2: any => match
        | throw::(err: any) => {
            discard println("Inner handler caught: " + err);
            // Re-throw to outer handler
            perform! throw::("Re-thrown from inner: " + err)
        }
        | panic;
    discard println("Before inner throw");
    discard perform! throw::("ValueError::Invalid input");
    discard println("should_not_reach");
    ()
};

discard println("");


// Example 2: Selective handling at different levels
discard println("=== Example 2: Selective Error Handling ===");

discard println {
    handle with _k1: any => match
        | throw::(err: any) => {
            discard println("Outer: Caught - " + err);
            "recovered"
        }
        | panic;
    handle with dyn_rec inner: k2: any => match
        | divide_error::(err: any) => {
            discard println("Inner: Caught division error - " + err);
            handle with inner;
            k2 () // Return safe default
        }
        | panic;
    discard println("Starting computation...");
    discard perform! divide_error::("Cannot divide by zero");
    discard println("After error handling");
    ()
};

discard println("");


// Example 3: Try-catch with successful execution
discard println("=== Example 3: Success Path ===");

discard println! {
    handle with _k: any => match
        | throw::(err: any) => {
            discard println("Error: " + err);
            0
        }
        | return::(v: any) => {
            discard println("Success: returning " + display! v);
            v
        }
        | panic;
    let a: nat = 10;
    let b: nat = 20;
    let sum: nat = a + b;
    discard println("Computed sum: " + nat_to_string(sum));
    let result: nat = perform! return::(sum);
    discard println("Final result: " + nat_to_string(result));
    ()
};

discard println("");


// Example 4: Three-level nested handling
discard println("=== Example 4: Three Levels of Handling ===");

discard println {
    handle with _k3: any => match
        | throw::(err: any) => {
            discard println("Level 3 (final): Caught - " + err);
            "level3_handled"
        }
        | panic;
    handle with _k2: any => match
        | throw::(err: any) => {
            discard println("Level 2: Caught - " + err);
            perform! throw::("Level2 -> " + err)
        }
        | panic;
    handle with _k1: any => match
        | throw::(err: any) => {
            discard println("Level 1: Caught - " + err);
            perform! throw::("Level1 -> " + err)
        }
        | panic;
    discard println("Starting computation...");
    discard perform! throw::("Original error");
    ()
};

discard println("");


// Example 5: Practical example with division and validation
discard println("=== Example 5: Practical Operations ===");

discard {
    handle with _k: any => match
        | div_error::() => {
            discard println("Application: Handling division error");
            1
        }
        | val_error::() => {
            discard println("Application: Handling validation error");
            50
        }
        | panic;
    handle with _k: any => match
        | val_error::() => {
            discard println("Validator: Caught validation error");
            perform! val_error::()
        }
        | panic;
    handle with _k: any => match
        | div_error::() => {
            discard println("Calculator: Caught division error");
            perform! div_error::()
        }
        | panic;
    discard println("Starting operations...");
    let x: nat = 20;
    discard println("After division: " + nat_to_string(x));
    discard if x > 100
        then perform! val_error::()
        else println("Validation passed");
    discard println("Operations completed successfully");
    ()
};

discard println("");


// Example 6: With cleanup/finally behavior
discard println("=== Example 6: Cleanup Behavior ===");

discard println {
    handle with dyn_rec h: k: any => match
        | throw::(err: any) => {
            discard println("Error caught: " + err);
            "error_handled"
        }
        | cleanup::() => {
            discard println("Cleanup executed");
            handle with h;
            k ()
        }
        | panic;
    discard println("Starting operation...");
    discard perform! cleanup::();
    discard println("After cleanup");
    discard perform! throw::("An error occurred");
    discard println("This will not be reached");
    ()
};

discard println("");
discard println("=== All examples completed ===");
