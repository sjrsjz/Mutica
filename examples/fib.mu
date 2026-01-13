let f: lambda = dyn_rec f: match
    | 0 => 0
    | 1 => 1
    | n: nat => f(n - 1) + f(n - 2)
    | panic;
f(10)