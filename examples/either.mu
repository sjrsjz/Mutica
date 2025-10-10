// Either 类型示例 (用于错误处理)
let Left: any = value: any |-> Left::value;
let Right: any = value: any |-> Right::value;

// map_right: 只对 Right 值进行映射
let map_right: any = either: (Left::any | Right::any) |-> f: any |->
    match either
        | Left::(err: any) => Left(err)
        | Right::(val: any) => Right(f(val))
        | panic;

// 安全除法
let safe_div: any = a: int |-> b: int |->
    match b
        | 0 => Left("Division by zero")
        | b_val: int => Right(a / b_val)
        | panic;

// 链式操作
let bind: any = either: (Left::any | Right::any) |-> f: any |->
    match either
        | Left::(err: any) => Left(err)
        | Right::(val: any) => f(val)
        | panic;

// 测试
let result1: any = safe_div(10)(2);
let result2: any = safe_div(10)(0);

// 使用 map_right 映射成功值
let mapped_result: any = map_right(result1)(x: int |-> x * 2);

// 使用 bind 进行链式操作
let chained_result: any = bind(result1)(x: int |-> safe_div(x)(2));

result1, result2, mapped_result, chained_result
