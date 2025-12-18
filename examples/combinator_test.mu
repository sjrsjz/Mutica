// Combinator Library 使用示例

let constraint {
    id::(id: any) &
    const::(const: any) &
    compose::(compose: any) &
    pipe::(pipe: any) &
    apply_twice::(apply_twice: any) &
    not::(not: any) &
    fst::(fst: any) &
    snd::(snd: any) &
    swap::(swap: any) &
    inc::(inc: any) &
    double::(double: any) &
    square::(square: any) &
    repeat_apply::(repeat_apply: any) &
    equal::(equal: any)
} = import "lib/combinator.mu";

// 测试恒等函数
discard print!"Identity: ";
discard println!(id(42));

// 测试常量函数
discard print!"Constant: ";
discard println!(const(5)(10));  // 返回 5，忽略 10

// 测试函数组合
discard print!"Compose (double ∘ inc): ";
let constraint double_then_inc: any = compose(double)(inc);
discard println!(double_then_inc(5));  // (5+1)*2 = 12

// 测试管道
discard print!"Pipe: ";
discard println!(pipe(5)(inc));  // 5 |> inc = 6

// 测试应用两次
discard print!"Apply twice (inc): ";
discard println!(apply_twice(inc)(5));  // inc(inc(5)) = 7

// 测试逻辑非
discard print!"Not true: ";
discard println!(not(true));

// 测试元组操作
discard print!"First of (1,2): ";
discard println!(fst((1, 2)));

discard print!"Second of (1,2): ";
discard println!(snd((1, 2)));

discard print!"Swap (1,2): ";
let constraint swapped: any = swap((1, 2));
discard print!(fst(swapped));
discard print!' ';
discard println!(snd(swapped));

// 测试数值函数
discard print!"Inc 5: ";
discard println!(inc(5));

discard print!"Double 5: ";
discard println!(double(5));

discard print!"Square 5: ";
discard println!(square(5));

// 测试重复应用
discard print!"Repeat apply inc 3 times on 0: ";
discard println!(repeat_apply(3)(inc)(0));  // 0 -> 1 -> 2 -> 3

// 测试相等比较
discard print!"5 == 5: ";
discard println!(equal(5)(5));

discard print!"5 == 3: ";
discard println!(equal(5)(3));

// 组合示例：链式函数调用
discard print!"Chain: inc -> double -> square on 2: ";
let constraint chain_fn: any = compose(square)(compose(double)(inc));
discard println!(chain_fn(2));  // (2+1)*2 = 6, 6^2 = 36

()
