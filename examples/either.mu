let constraint Any::(Any: any) = import "lib/any.mu";
// Either 类型示例 (用于错误处理)
let constraint Left: any = constraint value: any => Left::value;
let constraint Right: any = constraint value: any => Right::value;
let constraint Either: any = constraint  (T: any, U: any) => (Left T | Right U);
// map_right: 只对 Right 值进行映射
let constraint map_right: any = constraint either: Either(Any, Any) => constraint f: any =>
    match either
        | constraint Left::(err: any) => Left(err)
        | constraint Right::(val: any) => Right(f(val))
        | panic;

// 安全除法
let constraint safe_div: any = constraint a: nat => match
    | assert 0 => Left("Division by zero")
    | constraint b_val: nat => Right(a / b_val)
    | panic;

// 链式操作
let constraint bind: any = constraint either: Either(Any, Any) => constraint f: any =>
    match either
        | constraint Left::(err: any) => Left(err)
        | constraint Right::(val: any) => f(val)
        | panic;

// 测试
let constraint result1: any = safe_div(10)(2);
let constraint result2: any = safe_div(10)(0);

// 使用 map_right 映射成功值
let constraint mapped_result: any = map_right(result1)(constraint x: nat => x * 2);
// 使用 bind 进行链式操作
let constraint chained_result: any = bind(result1)(constraint x: nat => safe_div(x)(2));

result1, result2, mapped_result, chained_result
