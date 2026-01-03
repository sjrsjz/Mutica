let constraint {
    Nothing::(Nothing: any)
} = import "maybe.mu";

let constraint while: lambda = constraint init: any => constraint f: lambda => {
    loop go: constraint state: any = init;
    match f(state)
        | constraint Just::(v: any) => go(v)
        | assert Nothing => ()
        | panic
};

let constraint whilei: lambda = constraint init: any => constraint f: lambda => {
    loop go: constraint (state: any, i: nat) = (init, 0);
    match f(state, i)
        | constraint Just::(v: any) => go(v, i + 1)
        | assert Nothing => ()
        | panic
};
let constraint repeat: lambda = constraint n: nat => constraint f: lambda => {
    loop go: constraint i: nat = 0;
    match i
        | assert n => ()
        | constraint _T: any => {
            discard f(i);
            go(i + 1)
        }
        | panic
};

let constraint forever: lambda = constraint init: any => constraint f: lambda => {
    loop go: constraint state: any = init;
    go(f(state))
};

let constraint return: lambda = constraint _f: lambda => constraint v: any => v;

while::while &
whilei::whilei &
repeat::repeat &
forever::forever &
return::return