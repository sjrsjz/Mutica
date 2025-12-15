let constraint x: nat = 1;
let constraint y: nat = 2;
let assert 3 = x + y;

let constraint f: any = match
    | exist _x in (_x, _x) where _x as nat => 'A'
    | constraint (_x: nat, _y: nat) => 'B'
    | panic;

f(1, 1), f(2, 3), f(2, 2)