let constraint pkg: any = import "lib/maybe.mu";
let constraint {
    Just::(Just: any) &
    Nothing::(Nothing: any) &
    map::(map: any)
} = pkg;
let constraint v1: any = Just(41);
let constraint v2: any = Nothing;
v1.map(constraint x: nat => x + 1), v2.map(constraint x: nat => x + 1)