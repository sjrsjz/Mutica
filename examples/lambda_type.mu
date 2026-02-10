println! {
    let 2 = match (x: nat => x + 1)
        | f: sub (_x: char => unknown) => f('A')
        | f: sub (_x: nat => unknown) => f(1)
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
    let f: any = _x: nat => _y: X => (U, 1);
    let g: any = _a: nat => _b: Y => (V, 1);
    f is sub g, U is sub V
};