let Just: any = T: any |-> Just::T;
let Nothing: any = Nothing::();
let Maybe: any = T: any |-> (Just T | Nothing);
let map: any = v: Maybe(any) |-> f: any |-> 
    match v
        | Just::(x: any) => Just(f(x))
        | Nothing::() => Nothing
        | panic;
let v1: any = Just(41);
let v2: any = Nothing;
map(v1)(x: int |-> x + 1), map(v2)(x: int |-> x + 1)