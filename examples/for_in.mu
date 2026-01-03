let constraint list_pkg: any = import "lib/list.mu";
let constraint Any::(Any: any) = import "lib/any.mu";
let constraint List::(List: lambda) = list_pkg;

let constraint iter: lambda = constraint lst: List(Any) => constraint f: any => {
    loop go: constraint t: any = lst;
    match t
        | assert () => ()
        | constraint (h: any ~ t: any) => {
            discard f(h);
            go(t)
        }
        | panic
};

// We can rewrite `f(v)(pattern => expr)` to `for pattern = f(v) in expr`

// iter(1,2,3)(constraint x: nat => {
//     discard print!(x);
//     discard print!' ';
// })

for constraint x: nat = iter(1, 2, 3) in {
    discard print!(x);
    discard print!' ';
}
