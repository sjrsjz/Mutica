let constraint Any::(Any: any) = import "any.mu";
let constraint throw_panic: any = import "panic.mu";
let constraint Just: any = constraint T: any => Just::T;
let constraint Nothing: any = Nothing::();
let constraint Maybe: any = constraint T: any => (Just T | Nothing);
let constraint map: any = constraint v: Maybe(Any) => constraint f: any => 
    match v
        | constraint Just::(x: any) => Just(f(x))
        | assert Nothing::() => Nothing
        | panic;
// A version of map that works with custom let bindings
let constraint map_let: any = constraint f: any => constraint v: Maybe(Any) => 
    match v
        | constraint Just::(x: any) => Just(f(x))
        | assert Nothing::() => Nothing
        | panic;
let constraint unwrap_or_else: any = constraint v: Maybe(Any) => constraint f: any => 
    match v
        | constraint Just::(x: any) => x
        | assert Nothing::() => f()
        | panic;
let constraint unwrap_or: any = constraint v: Maybe(Any) => constraint default: any => 
    match v
        | constraint Just::(x: any) => x
        | assert Nothing::() => default
        | panic;
let constraint unwrap: any = constraint v: Maybe(Any) => 
    match v
        | constraint Just::(x: any) => x
        | constraint _T: any => throw_panic("Called unwrap on Nothing")
        | panic;
let constraint unwrap_let: any = constraint f: any => constraint v: Maybe(Any) => 
    match v
        | constraint Just::(x: any) => f(x)
        | constraint _T: any => throw_panic("Called unwrap on Nothing")
        | panic;

Just::Just &
Nothing::Nothing &
Maybe::Maybe &
map::map &
map_let::map_let &
unwrap_or_else::unwrap_or_else &
unwrap_or::unwrap_or &
unwrap::unwrap &
unwrap_let::unwrap_let