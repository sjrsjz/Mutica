discard {
    let constraint println::(println: any) = import "lib/string.mu";
    let constraint alloc: any = constraint f: any => constraint v: any => {
        let constraint pointer: any = alloc! v;
        let constraint result: any = f pointer;
        discard dealloc! pointer;
        result
    };

    let constraint my_str: any = #alloc "Hello, World!";
    discard println! my_str; // (0, 0) (represents as (Unit, Unit) internally)
    discard println(get! my_str);
    discard set!(my_str, "Goodbye, World!");
    discard println(get! my_str);
};

discard {
    let constraint Any::(Any: any) = import "lib/any.mu";
    let constraint {
        String::(String: any) &
        println::(println: any) &
        nat_to_string::(nat_to_string: any)
    } = import "lib/string.mu";
    let constraint Ok: any = constraint T: any => Ok::T;
    let constraint Err: any = constraint E: any => Err::E;
    let constraint Result: any = constraint (T: any, E: any) => (Ok T | Err E);
    let constraint try: any = constraint continuation: any => constraint res: Result(Any, Any) => 
        match res
            | constraint Ok::(x: any) => continuation(x)
            | constraint Err::(e: any) => Err(e)
            | panic;

    let constraint assert_nonzero: any = match
        | assert 0 => Err("Zero value")
        | constraint n: nat => Ok(n)
        | panic;

    let constraint divide: any = constraint a: nat => constraint b: nat => {
        let constraint b: nat = #try assert_nonzero b;
        Ok(a / b)
    };

    let constraint result1: Result(nat, String) = divide 10 2;
    let constraint result2: Result(nat, String) = divide 10 0;
    let constraint print_result: any = constraint res: Result(nat, String) => 
        match res
            | constraint Ok::(x: nat) => println("Result: " + nat_to_string x)
            | constraint Err::(e: String) => println("Error: " + e)
            | panic;
    discard print_result result1;
    discard print_result result2;
};