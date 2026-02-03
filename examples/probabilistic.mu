let {
    println::(println: any)
} = import "lib/string.mu";
let deref::(deref: any) = import "lib/mutable.mu";

let bool_to_string: any = b: any => 
    if b then "true" else "false";

let program: any = result: mut () => {
    let a: any = perform! flip::();
    let b: any = perform! flip::();
    let c: any = perform! flip::();
    println("Flips: " + bool_to_string(a) + ", " + bool_to_string(b) + ", " + bool_to_string(c));
    result := deref result + ((a && b) || (a && c),);
};

let program2: any = result: mut () => {
    program(result);
    // println("This line will be printed if an exception occurs above and stack is unwound correctly.");
};

let handler: any = dyn_rec h: k: any => match
    | flip::() => {
        { handle with h; k true };
        { handle with h; k false };
    }
    | v: any => {
        let r: any = perform! v;
        handle with h;
        k r
    }
    | panic;

let result: any = {
    let result: any = mut ();
    {
        handle with handler;
        program2(result);
    };
    result
};
println("Result of probabilistic program: " + display!(result));