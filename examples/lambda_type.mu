println! {
    let 2 = match (x: nat => x + 1)
        | f: (lambda | x: char | panic) => f('A')
        | f: (lambda | x: nat | panic) => f(1)
        | panic;

    let f: any = [A::() | B::() | C::()] => [X::() | Y::() | Z::()];
    let g: any = [A::() | B::() | C::()] => [X::() | Y::() | Z::()];
    let u: any = [A::() | B::()] => [X::() | Y::() | Z::()];
    let v: any = [A::() | B::() | C::()] => [X::() | Y::()];
    f == g, f != u, f != v, u is sub f, v is sub f, f is sub u, f is sub v
};

println! {
    let U: any = A::();
    let V: any = A::() | B::();
    let X: any = C::() | D::();
    let Y: any = C::();
    let f: any = x: nat => y: X => (U, 1);
    let g: any = a: nat => b: Y => (V, 1);
    f is sub g, U is sub V
};