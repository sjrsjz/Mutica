// 阶乘和斐波那契数列示例

// 普通递归阶乘
let constraint factorial: any = 
    dyn_rec fact: match
        | assert 0 => 1
        | assert 1 => 1
        | constraint n: nat => n * fact(n - 1)
        | panic;

// 尾递归阶乘
let constraint factorial_tail: any = constraint n: nat => [
        let constraint helper: any = dyn_rec h: constraint acc: nat => match 
            | assert 0 => acc
            | assert 1 => acc
            | constraint n: nat => h(acc * n)(n - 1)
            | panic;
        helper(1)(n)
    ];

// 斐波那契数列
let constraint fibonacci: any = 
    dyn_rec fib: match 
        | assert 0 => 0
        | assert 1 => 1
        | constraint n: nat => fib(n - 1) + fib(n - 2)
        | panic;

// 尾递归斐波那契
let constraint fibonacci_tail: any = constraint n: nat => [
    let constraint helper: any = dyn_rec helper: constraint a: nat => constraint b: nat => match
            | assert 0 => a
            | constraint n: nat => helper(b)(a + b)(n - 1)
            | panic;
        helper(0)(1)(n)
    ];

// 测试
factorial(5), factorial_tail(5), fibonacci(7), fibonacci_tail(7)
