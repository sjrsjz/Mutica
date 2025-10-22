let handler: any = match
    | GetA::() => 42
    | GetB::() => 84
    | panic;
handle z: int = 1 with handler;
let x: int = perform! GetA::();
let y: int = perform! GetB::();
x, y, z