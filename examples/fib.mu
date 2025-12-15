let constraint f: any = dyn_rec f: match
    | assert 0 => 0
    | assert 1 => 1
    | constraint n: nat => f(n - 1) + f(n - 2)
    | panic;
f(10)