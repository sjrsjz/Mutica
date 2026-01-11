discard {
    let constraint handler: lambda = dyn_rec handler: constraint k: any => match
        | assert GetA::() => {
            handle with handler;
            k 42
        }
        | assert GetB::() => {
            handle with handler;
            k 84
        }
        | panic;
    handle constraint z: nat = 1 with handler;
    let constraint x: nat = perform! GetA::();
    let constraint y: nat = perform! GetB::();
    discard println!(x, y, z);
};

discard {
    let constraint {
        println::(println: lambda)
    } = import "lib/string.mu";
    let constraint handler: lambda = dyn_rec handler: constraint k: any => match
        | constraint throw::(v: any) => {
            discard println("Caught throw with value: " + display! v);
        }
        | constraint debug::(v: any ~ payload: any) => {
            discard println("Debug: " + v);
            handle with handler;
            k payload
        }
        | panic;
    handle with handler;
    let constraint (x: nat, y: nat) = perform! debug::("Performing debug for x", 2, 3);
    discard println("Final values: " + display! x + ", " + display! y);
    discard perform! throw::("An error occurred");
    discard println("This line will not be reached.");
};