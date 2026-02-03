{
    let handler: any = dyn_rec handler: k: any => match
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
    println!(x, y, z);
};

{
    let {
        println::(println: any)
    } = import "lib/string.mu";
    handle with dyn_rec handler: k: any => match
        | throw::(v: any) => {
            println("Caught throw with value: " + display! v);
        }
        | debug::(v: any ~ payload: any) => {
            println("Debug: " + v);
            handle with handler;
            k payload
        }
        | panic;
    let (x: nat, y: nat) = perform! debug::("Performing debug for x", 2, 3);
    println("Final values: " + display! x + ", " + display! y);
    perform! throw::("An error occurred");
    println("This line will not be reached.");
};