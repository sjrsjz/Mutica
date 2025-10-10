// 元组/配对操作示例
let Pair: any = fst: any |-> snd: any |-> Pair::(fst, snd);

// 获取第一个元素
let fst: any = pair: Pair::(any, any) |->
    match pair
        | Pair::(first: any, any) => first
        | panic;

// 获取第二个元素
let snd: any = pair: Pair::(any, any) |->
    match pair
        | Pair::(any, second: any) => second
        | panic;

// 交换元素
let swap: any = pair: Pair::(any, any) |->
    match pair
        | Pair::(first: any, second: any) => Pair(second)(first)
        | panic;

// 对两个元素应用函数
let map_both: any = pair: Pair::(any, any) |-> f: any |->
    match pair
        | Pair::(first: any, second: any) => Pair(f(first))(f(second))
        | panic;

// 创建配对
let p1: any = Pair(10)(20);
let p2: any = Pair("Hello")("World");

// 测试
fst(p1),
snd(p1),
swap(p1),
map_both(Pair(3)(4))(x: int |-> x * x),
fst(p2),
snd(p2)
