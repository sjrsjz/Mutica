// 阶乘和斐波那契数列示例

// 普通递归阶乘
let factorial: lambda = 
    dyn_rec fact: match
        | 0 => 1
        | 1 => 1
        | n: nat => n * fact(n - 1)
        | panic;

// 尾递归阶乘
let factorial_tail: lambda = n: nat => [
        let helper: lambda = dyn_rec h: acc: nat => match 
            | 0 => acc
            | 1 => acc
            | n: nat => h(acc * n)(n - 1)
            | panic;
        helper(1)(n)
    ];

// 斐波那契数列
let fibonacci: lambda = 
    dyn_rec fib: match 
        | 0 => 0
        | 1 => 1
        | n: nat => fib(n - 1) + fib(n - 2)
        | panic;

// 尾递归斐波那契
let fibonacci_tail: lambda = n: nat => [
    let helper: lambda = dyn_rec helper: a: nat => b: nat => match
            | 0 => a
            | n: nat => helper(b)(a + b)(n - 1)
            | panic;
        helper(0)(1)(n)
    ];

// 测试
factorial(5), factorial_tail(5), fibonacci(7), fibonacci_tail(7)
