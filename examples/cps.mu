discard {
    let constraint println::(println: lambda) = import "lib/string.mu";
    let constraint Pointer: any = (nat, nat);
    let constraint alloc: lambda = constraint f: lambda => constraint v: any => {
        let constraint pointer: Pointer = alloc! v;
        let constraint result: any = f pointer;
        discard dealloc! pointer;
        result
    };

    let constraint my_str: Pointer = #alloc "Hello, World!";
    discard println! my_str; // (0, 0) (represents as (Unit, Unit) internally)
    discard println(get! my_str);
    discard set!(my_str, "Goodbye, World!");
    discard println(get! my_str);
};

discard {
    let constraint Any::(Any: any) = import "lib/any.mu";
    let constraint {
        String::(String: any) &
        println::(println: lambda) &
        nat_to_string::(nat_to_string: lambda)
    } = import "lib/string.mu";
    let constraint Ok: lambda = constraint T: any => Ok::T;
    let constraint Err: lambda = constraint E: any => Err::E;
    let constraint Result: lambda = constraint (T: any, E: any) => (Ok T | Err E);
    let constraint try: lambda = constraint continuation: lambda => constraint res: Result(Any, Any) => 
        match res
            | constraint Ok::(x: any) => continuation(x)
            | constraint Err::(e: any) => Err(e)
            | panic;

    let constraint assert_nonzero: lambda = match
        | assert 0 => Err("Zero value")
        | constraint n: nat => Ok(n)
        | panic;

    let constraint divide: lambda = constraint a: nat => constraint b: nat => {
        let constraint b: nat = #try assert_nonzero b;
        Ok(a / b)
    };

    let constraint result1: Result(nat, String) = divide 10 2;
    let constraint result2: Result(nat, String) = divide 10 0;
    let constraint print_result: lambda = constraint res: Result(nat, String) => 
        match res
            | constraint Ok::(x: nat) => println("Result: " + nat_to_string x)
            | constraint Err::(e: String) => println("Error: " + e)
            | panic;
    discard print_result result1;
    discard print_result result2;
};