let x: nat = 1;
let y: nat = 2;
let 3 = x + y;

let f: any = match
    | exist (_x, _x) where _x: nat => 'A'
    | (_x: nat, _y: nat) => 'B'
    | panic;

f(1, 1), f(2, 3), f(2, 2)