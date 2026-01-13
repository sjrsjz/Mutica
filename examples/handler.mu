discard {
    let handler: lambda = dyn_rec handler: k: any => match
        | GetA::() => {
            handle with handler;
            k 42
        }
        | GetB::() => {
            handle with handler;
            k 84
        }
        | panic;
    handle z: nat = 1 with handler;
    let x: nat = perform! GetA::();
    let y: nat = perform! GetB::();
    discard println!(x, y, z);
};

discard {
    let {
        println::(println: lambda)
    } = import "lib/string.mu";
    let handler: lambda = dyn_rec handler: k: any => match
        | throw::(v: any) => {
            discard println("Caught throw with value: " + display! v);
        }
        | debug::(v: any ~ payload: any) => {
            discard println("Debug: " + v);
            handle with handler;
            k payload
        }
        | panic;
    handle with handler;
    let (x: nat, y: nat) = perform! debug::("Performing debug for x", 2, 3);
    discard println("Final values: " + display! x + ", " + display! y);
    discard perform! throw::("An error occurred");
    discard println("This line will not be reached.");
};