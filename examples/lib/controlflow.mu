let {
    Nothing::(Nothing: any)
} = import "maybe.mu";

let while: lambda = init: any => f: lambda => {
    loop go: state: any = init;
    match f(state)
        | Just::(v: any) => go(v)
        | Nothing => ()
        | panic
};

let while_condition: lambda = condition: lambda => body: lambda => {
    loop go: assert () = ();
    if condition() then {
        discard body();
        go()
    } else ()
};

let whilei: lambda = init: any => f: lambda => {
    loop go: (state: any, i: nat) = (init, 0);
    match f(state, i)
        | Just::(v: any) => go(v, i + 1)
        | Nothing => ()
        | panic
};

let repeat: lambda = n: nat => f: lambda => {
    loop go: i: nat = 0;
    match i
        | n => ()
        | _T: any => {
            discard f(i);
            go(i + 1)
        }
        | panic
};

let forever: lambda = init: any => f: lambda => {
    loop go: state: any = init;
    go(f(state))
};

let return: lambda = _f: lambda => v: any => v;

while::while &
while_condition::while_condition &
whilei::whilei &
repeat::repeat &
forever::forever &
return::return