let list_pkg: any = import "lib/list.mu";
let List::(List: any) = list_pkg;

let List: any = List;
let iter: any = lst: List(any) => f: any => {
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

// iter(1, 2, 3)(x: nat => {
//     discard print!(x);
// })

for x: nat = iter(1, 2, 3) in {
    discard print!(x);
    discard print!' ';
}
