let fib: any = rec f: match
    | 0 => 0
    | 1 => 1
    | n: any => f(n - 1) + f(n - 2)
    | panic;
fib(28)