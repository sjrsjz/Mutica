let pkg: any = import "lib/maybe.mu";
let {
    Just::(Just: any) &
    Nothing::(Nothing: any) &
    map::(map: any)
} = pkg;
let v1: any = Just(41);
let v2: any = Nothing;
map(v1)(x: nat => x + 1), map(v2)(x: nat => x + 1)