let constraint Any::(Any: any) = import "any.mu";
let constraint throw_panic::(throw_panic: lambda) = import "panic.mu";
let constraint Just: lambda = constraint T: any => Just::T;
let constraint Nothing: any = Nothing::();
let constraint Maybe: lambda = constraint T: any => (Just T | Nothing);
let constraint map: lambda = constraint v: Maybe(Any) => constraint f: lambda => 
    match v
        | constraint Just::(x: any) => Just(f(x))
        | assert Nothing::() => Nothing
        | panic;
// A version of map that works with custom let bindings
let constraint map_let: lambda = constraint f: lambda => constraint v: Maybe(Any) => 
    match v
        | constraint Just::(x: any) => Just(f(x))
        | assert Nothing::() => Nothing
        | panic;
let constraint unwrap_or_else: lambda = constraint v: Maybe(Any) => constraint f: lambda => 
    match v
        | constraint Just::(x: any) => x
        | assert Nothing::() => f()
        | panic;
let constraint unwrap_or: lambda = constraint v: Maybe(Any) => constraint default: any => 
    match v
        | constraint Just::(x: any) => x
        | assert Nothing::() => default
        | panic;
let constraint unwrap: lambda = constraint v: Maybe(Any) => 
    match v
        | constraint Just::(x: any) => x
        | constraint _T: any => throw_panic("Called unwrap on Nothing")
        | panic;
let constraint unwrap_let: lambda = constraint f: lambda => constraint v: Maybe(Any) => 
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