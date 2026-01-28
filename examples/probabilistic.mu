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
    discard println("Flips: " + bool_to_string(a) + ", " + bool_to_string(b) + ", " + bool_to_string(c));
    discard result := deref result + ((a && b) || (a && c),);
};

let program2: any = result: mut () => {
    discard program(result);
    // discard println("This line will be printed if an exception occurs above and stack is unwound correctly.");
};

let handler: any = dyn_rec h: k: any => match
    | flip::() => {
        discard { handle with h; k true };
        discard { handle with h; k false };
    }
    | v: any => {
        let r: any = perform! v;
        handle with h;
        k r
    }
    | panic;

let result: any = {
    let result: any = mut ();
    discard {
        handle with handler;
        discard program2(result);
    };
    result
};
discard println("Result of probabilistic program: " + display!(result));