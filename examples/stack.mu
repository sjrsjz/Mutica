let constraint {
    println::(println: lambda) &
    nat_to_string::(nat_to_string: lambda)
} = import "lib/string.mu";
let constraint throw_panic::(throw_panic: lambda) = import "lib/panic.mu";
let constraint {
    return::(return: lambda) &
    eval::(eval: lambda)
} = import "lib/state.mu";

// ==========================================
// 定义栈操作
// ==========================================

let constraint Cons: lambda = constraint (h: any, t: any) => (h, t);
let constraint Nil: () = ();

let constraint push: lambda = constraint f: lambda => constraint v: any => constraint s: any => constraint k: lambda => f()(Cons(v, s))(k);

let constraint pop: lambda = constraint f: lambda => assert () => constraint s: any => constraint k: lambda => {
    match s
        | constraint (h: any, t: any) => f(h)(t)(k)
        | assert () => throw_panic("Empty stack") // 空栈 Panic
        | panic
};

let constraint top: lambda = constraint f: lambda => assert () => constraint s: any => constraint k: lambda => {
    match s
        | constraint (h: any, _t: any) => f(h)(s)(k) // 注意这里 s 没变
        | assert () => throw_panic("Empty stack") // 空栈 Panic
        | panic
};


// ==========================================
// 用户代码：完全命令式风格
// ==========================================

let constraint stack_program: lambda = {
    // 尽管可能很奇怪，但是我们需要一个`discard ()`去封装作用域，或者最起码把stack_program写为 (() => { ... })() 形式，否则手动cps会把上级表达式变换掉
    // 这是一个无法修复也不应当修复的特性，它实际上不是语言的bug，而是因为花括号不会创建新的作用域导致的，除非花括号被设计为隐式`discard ();`，但那样就导致花括号的语义变更了
    // Mutica只存在函数作用域和泛型参数作用域，没有块作用域
    discard ();
    // push(10);
    discard #push(10);
    // push(20);
    discard #push(20);
    // let peek = top(); // 应该是 20
    let constraint peek: nat = #top(); 
    // push(30);
    discard #push(30);    
    return (peek, #pop(), #pop(), #pop())
};

// ==========================================
// 运行
// ==========================================

let constraint initial_stack: () = Nil; // 初始为空栈

for constraint (p: nat, a: nat, b: nat, c: nat) = stack_program.eval(initial_stack) in {
    discard println("Peek (expect 20): " + nat_to_string(p));
    discard println("Pop 1 (expect 30): " + nat_to_string(a));
    discard println("Pop 2 (expect 20): " + nat_to_string(b));
    discard println("Pop 3 (expect 10): " + nat_to_string(c));
}
