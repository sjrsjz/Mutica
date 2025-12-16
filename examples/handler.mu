let constraint handler: any = match
    | assert GetA::() => 42
    | assert GetB::() => 84
    | panic;
handle constraint z: nat = 1 with handler;
let constraint x: nat = perform! GetA::();
let constraint y: nat = perform! GetB::();
x, y, z