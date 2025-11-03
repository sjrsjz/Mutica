let pkg: any = import "lib/maybe.mu";
let Just::(Just: any) = pkg;
let Nothing::(Nothing: any) = pkg;
let map::(map: any) = pkg;
let v1: any = Just(41);
let v2: any = Nothing;
map(v1)(x: int => x + 1), map(v2)(x: int => x + 1)