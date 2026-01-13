let list_pkg: any = import "lib/list.mu";
let Any::(Any: any) = import "lib/any.mu";
let List::(List: lambda) = list_pkg;

let iter: lambda = lst: List(Any) => f: any => {
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

// iter(1,2,3)(x: nat => {
//     discard print!(x);
//     discard print!' ';
// })

for x: nat = iter(1, 2, 3) in {
    discard print!(x);
    discard print!' ';
}
