let fib: any = rec f: match
    | eq 0 => 0
    | eq 1 => 1
    | n: nat => f(n - 1) + f(n - 2)
    | panic;
fib(28)