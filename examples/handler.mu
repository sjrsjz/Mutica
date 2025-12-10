let handler: any = match
    | GetA::() => 42
    | GetB::() => 84
    | panic;
handle z: nat = 1 with handler;
let x: nat = perform! GetA::();
let y: nat = perform! GetB::();
x, y, z