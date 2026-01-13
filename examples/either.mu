let Any::(Any: any) = import "lib/any.mu";
let String::(String: any) = import "lib/string.mu";
// Either 类型示例 (用于错误处理)
let Left: lambda = value: any => Left::value;
let Right: lambda = value: any => Right::value;
let Either: lambda = (T: any, U: any) => (Left T | Right U);
// map_right: 只对 Right 值进行映射
let map_right: lambda = either: Either(Any, Any) => f: lambda =>
    match either
        | Left::(err: any) => Left(err)
        | Right::(val: any) => Right(f(val))
        | panic;

// 安全除法
let safe_div: lambda = a: nat => match
    | assert 0 => Left("Division by zero")
    | b_val: nat => Right(a / b_val)
    | panic;

// 链式操作
let bind: lambda = either: Either(Any, Any) => f: lambda =>
    match either
        | Left::(err: any) => Left(err)
        | Right::(val: any) => f(val)
        | panic;

// 测试
let result1: Either(String, nat) = safe_div 10 2;
let result2: Either(String, nat) = safe_div 10 0;

// 使用 map_right 映射成功值
let mapped_result: Either(String, nat) = map_right(result1)(x: nat => x * 2);
// 使用 bind 进行链式操作
let chained_result: Either(String, nat) = bind(result1)(x: nat => safe_div(x)(2));

result1, result2, mapped_result, chained_result
