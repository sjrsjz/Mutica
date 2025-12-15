let constraint pkg: any = import "lib/maybe.mu";
let constraint {
    Just::(Just: any) &
    Nothing::(Nothing: any) &
    map::(map: any)
} = pkg;
let constraint v1: any = Just(41);
let constraint v2: any = Nothing;
map(v1)(constraint x: nat => x + 1), map(v2)(constraint x: nat => x + 1)