let throw_panic::(throw_panic: any) = import "panic.mu";
let Just: any = T: any => Just::T;
let Nothing: any = Nothing::();
let Maybe: any = T: any => (Just T | Nothing);
let map: any = v: Maybe(any) => f: sub (_v: never => unknown) => 
    match v
        | Just::(x: any) => Just(f(x))
        | Nothing::() => Nothing
        | panic;
// A version of map that works with custom let bindings
let map_let: any = f: sub (_v: never => unknown) => v: Maybe(any) => 
    match v
        | Just::(x: any) => Just(f(x))
        | Nothing::() => Nothing
        | panic;
let unwrap_or_else: any = v: Maybe(any) => f: sub (() => unknown) => 
    match v
        | Just::(x: any) => x
        | Nothing::() => f()
        | panic;
let unwrap_or: any = v: Maybe(any) => default: any => 
    match v
        | Just::(x: any) => x
        | Nothing::() => default
        | panic;
let unwrap: any = v: Maybe(any) => 
    match v
        | Just::(x: any) => x
        | _T: any => throw_panic("Called unwrap on Nothing")
        | panic;
let unwrap_let: any = f: sub (_v: never => unknown) => v: Maybe(any) => 
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