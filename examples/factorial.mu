// 阶乘和斐波那契数列示例

// 普通递归阶乘
let factorial: any = 
    rec fact: n: int |->
        match n
            | 0 => 1
            | 1 => 1
            | ! => n * fact(n - 1);

// 尾递归阶乘
let factorial_tail: any = n: int |-> [
        let helper: any = rec h: acc: int |-> n: int |->
            match n
                | 0 => acc
                | 1 => acc
                | ! => h(acc * n)(n - 1);
        helper(1)(n)
    ];

// 斐波那契数列
let fibonacci: any = 
    rec fib: n: int |->
        match n
            | 0 => 0
            | 1 => 1
            | ! => fib(n - 1) + fib(n - 2);

// 尾递归斐波那契
let fibonacci_tail: any = n: int |-> [
    let helper: any = rec helper: a: int |-> b: int |-> n: int |->
        match n
            | 0 => a
            | ! => helper(b)(a + b)(n - 1);
        helper(0)(1)(n)
    ];

// 测试
factorial(5), factorial_tail(5), fibonacci(7), fibonacci_tail(7)
