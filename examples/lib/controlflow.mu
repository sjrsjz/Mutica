let constraint {
    Nothing::(Nothing: any)
} = import "maybe.mu";

let constraint while: any = constraint init: any => constraint f: any => {
    loop go: constraint state: any = init;
    match f(state)
        | constraint Just::(v: any) => go(v)
        | assert Nothing => ()
        | panic
};

let constraint whilei: any = constraint init: any => constraint f: any => {
    loop go: constraint (state: any, i: nat) = (init, 0);
    match f(state, i)
        | constraint Just::(v: any) => go(v, i + 1)
        | assert Nothing => ()
        | panic
};
let constraint repeat: any = constraint n: nat => constraint f: any => {
    loop go: constraint i: nat = 0;
    match i
        | assert n => ()
        | constraint _T: any => {
            discard f(i);
            go(i + 1)
        }
        | panic
};

let constraint forever: any = constraint init: any => constraint f: any => {
    loop go: constraint state: any = init;
    go(f(state))
};

let constraint return: any = constraint _f: any => constraint v: any => v;

while::while &
whilei::whilei &
repeat::repeat &
forever::forever &
return::return