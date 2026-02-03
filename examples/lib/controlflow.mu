let {
    Nothing::(Nothing: any)
} = import "maybe.mu";

let while: any = init: any => f: (lambda | v: never | panic) => {
    loop go: state: any = init;
    match f(state)
        | Just::(v: any) => go(v)
        | Nothing => ()
        | panic
};

let while_condition: any = condition: any => body: (lambda | () | panic) => {
    loop go: assert () = ();
    if condition() then {
        body();
        go()
    } else ()
};

let whilei: any = init: any => f: (lambda | (state: never, counter: nat) | panic) => {
    loop go: (state: any, i: nat) = (init, 0);
    match f(state, i)
        | Just::(v: any) => go(v, i + 1)
        | Nothing => ()
        | panic
};

let repeat: any = n: nat => f: (lambda | counter: nat | panic) => {
    loop go: i: nat = 0;
    match i
        | n => ()
        | _T: any => {
            f(i);
            go(i + 1)
        }
        | panic
};

let forever: any = init: any => f: (lambda | v: never | panic) => {
    loop go: state: any = init;
    go(f(state))
};

let return: any = _f: any => v: any => v;

while::while &
while_condition::while_condition &
whilei::whilei &
repeat::repeat &
forever::forever &
return::return