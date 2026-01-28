let list_pkg: any = import "lib/list.mu";
let List::(List: any) = list_pkg;

let iter: any = T: any => lst: List(T) => f: (lambda | v: T | panic) => {
    loop go: t: any = lst;
    match t
        | () => ()
        | (h: any ~ t: any) => {
            discard f(h);
            go(t)
        }
        | panic
};

// We can rewrite `f(v)(pattern => expr)` to `for pattern = f(v) in expr`

// nat.iter(1,2,3)(x: nat => {
//     discard print!(x);
//     discard print!' ';
// })

for x: nat = nat.iter(1, 2, 3) in {
    discard print!(x);
    discard print!' ';
}
