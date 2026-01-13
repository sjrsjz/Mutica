let Any::(Any: any) = import "any.mu";
let throw_panic::(throw_panic: lambda) = import "panic.mu";
let Just: lambda = T: any => Just::T;
let Nothing: any = Nothing::();
let Maybe: lambda = T: any => (Just T | Nothing);
let map: lambda = v: Maybe(Any) => f: lambda => 
    match v
        | Just::(x: any) => Just(f(x))
        | Nothing::() => Nothing
        | panic;
// A version of map that works with custom let bindings
let map_let: lambda = f: lambda => v: Maybe(Any) => 
    match v
        | Just::(x: any) => Just(f(x))
        | Nothing::() => Nothing
        | panic;
let unwrap_or_else: lambda = v: Maybe(Any) => f: lambda => 
    match v
        | Just::(x: any) => x
        | Nothing::() => f()
        | panic;
let unwrap_or: lambda = v: Maybe(Any) => default: any => 
    match v
        | Just::(x: any) => x
        | Nothing::() => default
        | panic;
let unwrap: lambda = v: Maybe(Any) => 
    match v
        | Just::(x: any) => x
        | _T: any => throw_panic("Called unwrap on Nothing")
        | panic;
let unwrap_let: lambda = f: lambda => v: Maybe(Any) => 
    match v
        | Just::(x: any) => f(x)
        | _T: any => throw_panic("Called unwrap on Nothing")
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