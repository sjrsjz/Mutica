let exist fib in fib where fib: any = rec f: match
    | assert 0 => 0
    | assert 1 => 1
    | exist n in n where n: nat => __add!(f(__sub!(n, 1)), f(__sub!(n, 2)))
    | panic;
fib(5)