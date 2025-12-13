let exist x in x where x: nat = 1;
let exist y in y where y: nat = 2;
let assert 3 = __add!(x, y);

let exist f in f where f: any = match
    | exist x in (x, x) where x: nat => 'A'
    | exist {x, y} in (x, y) where (x, y): (nat, nat) => 'B'
    | panic;

f(1, 1), f(2, 3), f(2, 2)