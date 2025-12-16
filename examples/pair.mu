// 元组/配对操作示例
let constraint Pair: any = constraint fst: any => constraint snd: any => Pair::(fst, snd);

// 获取第一个元素
let constraint fst: any = match
    | constraint Pair::(first: any, any) => first
    | panic;

// 获取第二个元素
let constraint snd: any = match 
    | constraint Pair::(any, second: any) => second
    | panic;

// 交换元素
let constraint swap: any = match
    | constraint Pair::(first: any, second: any) => Pair(second)(first)
    | panic;

// 对两个元素应用函数
let constraint map_both: any = constraint pair: Pair(any)(any) => constraint f: any =>
    match pair
        | constraint Pair::(first: any, second: any) => Pair(f(first))(f(second))
        | panic;

// 创建配对
let constraint p1: any = Pair(10)(20);
let constraint p2: any = Pair("Hello")("World");

// 测试
fst(p1),
snd(p1),
swap(p1),
map_both(Pair(3)(4))(constraint x: nat => x * x),
fst(p2),
snd(p2)
