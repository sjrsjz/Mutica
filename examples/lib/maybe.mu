let Just: any = T: any |-> Just::T;
let Nothing: any = Nothing::();
let Maybe: any = T: any |-> (Just T | Nothing);
let map: any = v: Maybe(any) |-> f: any |-> 
    match v
        | Just::(x: any) => Just(f(x))
        | Nothing::() => Nothing
        | panic;

Just::Just & Nothing::Nothing & Maybe::Maybe & map::map