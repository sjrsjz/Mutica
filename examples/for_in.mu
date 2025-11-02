let list_pkg: any = import "lib/list.mu";
let List: any = list_pkg.List;
let iter: any = lst: List(any) -> f: any -> {
    loop go: t: any = lst;
    match t
        | () => ()
        | (h: any) @ (t: any) => {
            discard f(h);
            go(t)
        }
        | panic
};

// We can rewrite `f(v)(pattern -> expr)` to `for pattern = f(v) in expr`

// iter(1, 2, 3)(x: int -> {
//     discard print!(x);
// })

for x: int = iter(1, 2, 3) in {
    discard print!(x);
}
