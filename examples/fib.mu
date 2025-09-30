let fib: any = 
    rec f: (n: int) |-> 
        match n
            | 0 => 0
            | 1 => 1
            | ! => f(n - 1) + f(n - 2);
fib(10)