let list_pkg: any = import "lib/list.mu";
let List::(List: any) = list_pkg;

let iter: any = T: any => lst: List(T) => f: sub (_v: T => unknown) => {
    loop go: t: any = lst;
    match t
        | () => ()
        | (h: any ~ t: any) => {
            f(h);
            go(t)
        }
        | panic
};

// We can rewrite `f(v)(pattern => expr)` to `for pattern = f(v) in expr`

// nat.iter(1,2,3)(x: nat => {
//     print!(x);
//     print!' ';
// })

for x: nat = nat.iter(1, 2, 3) in {
    print!(x);
    print!' ';
}
