// Combinator Library - 组合子库
// 包含常见的函数式编程组合子和高阶函数

// ============ 基础组合子 ============

// I combinator (Identity) - 恒等函数
let constraint id: any = constraint x: any => x;

// K combinator (Constant) - 常量函数
let constraint const: any = constraint x: any => constraint _y: any => x;

// S combinator - 组合应用
let constraint s: any = constraint f: any => constraint g: any => constraint x: any => f(x)(g(x));

// B combinator (Compose) - 函数组合
let constraint compose: any = constraint f: any => constraint g: any => constraint x: any => f(g(x));

// C combinator (Flip) - 翻转参数
let constraint flip: any = constraint f: any => constraint x: any => constraint y: any => f(y)(x);

// W combinator (Duplication) - 参数复制
let constraint dup: any = constraint f: any => constraint x: any => f(x)(x);

// ============ 函数应用组合子 ============

// 应用函数到值
let constraint apply: any = constraint f: any => constraint x: any => f(x);

// 反向应用（管道）
let constraint pipe: any = constraint x: any => constraint f: any => f(x);

// 函数应用两次
let constraint apply_twice: any = constraint f: any => constraint x: any => f(f(x));

// ============ 逻辑组合子 ============

// 逻辑非
let constraint not: any = match
    | assert true => false
    | assert false => true
    | panic;

// 逻辑与（短路求值）
let constraint and: any = match
    | assert true => constraint b: any => b
    | assert false => constraint _b: any => false
    | panic;

// 逻辑或（短路求值）
let constraint or: any = match
    | assert true => constraint _b: any => true
    | assert false => constraint b: any => b
    | panic;

// ============ 条件组合子 ============

// if-then-else 作为函数
let constraint if_then_else: any = constraint cond: any => constraint then_val: any => constraint else_val: any =>
    if cond then then_val else else_val;

// when: 条件为真时执行
let constraint when: any = constraint cond: any => constraint f: any =>
    if cond then f() else ();

// unless: 条件为假时执行
let constraint unless: any = constraint cond: any => constraint f: any =>
    if cond then () else f();

// ============ 函数组合工具 ============

// 三元组合
let constraint compose3: any = constraint f: any => constraint g: any => constraint h: any => constraint x: any =>
    f(g(h(x)));

// 四元组合
let constraint compose4: any = constraint f: any => constraint g: any => constraint h: any => constraint i: any => constraint x: any =>
    f(g(h(i(x))));

// ============ 函数修饰组合子 ============

// on: 在应用二元操作前先变换参数
let constraint on: any = constraint op: any => constraint f: any => constraint x: any => constraint y: any =>
    op(f(x))(f(y));

// fix: Y 组合子（不动点组合子）
let constraint fix: any = constraint f: any => {
    let constraint go: any = dyn_rec go: f(go);
    go
};

// ============ 数值组合子 ============

// 自增
let constraint inc: any = constraint x: nat => x + 1;

// 自减（自然数，最小为0）
let constraint dec: any = constraint x: nat => if x > 0 then x - 1 else 0;

// 倍增
let constraint double: any = constraint x: nat => x * 2;

// 平方
let constraint square: any = constraint x: nat => x * x;

// ============ 忽略操作 ============

// 忽略第一个参数
let constraint ignore_first: any = constraint _x: any => constraint y: any => y;

// 忽略第二个参数（同 const）
let constraint ignore_second: any = const;

// void: 执行函数但忽略返回值
let constraint void: any = constraint f: any => {
    discard f;
    ()
};

// ============ 元组操作组合子 ============

// fst: 获取二元组第一个元素
let constraint fst: any = constraint (x: any, _y: any) => x;

// snd: 获取二元组第二个元素
let constraint snd: any = constraint (_x: any, y: any) => y;

// swap: 交换二元组元素
let constraint swap: any = constraint (x: any, y: any) => (y, x);

// both: 对二元组的两个元素应用同一函数
let constraint both: any = constraint f: any => constraint (x: any, y: any) => (f(x), f(y));

// first: 只对二元组第一个元素应用函数
let constraint first: any = constraint f: any => constraint (x: any, y: any) => (f(x), y);

// second: 只对二元组第二个元素应用函数
let constraint second: any = constraint f: any => constraint (x: any, y: any) => (x, f(y));

// ============ 链式调用组合子 ============

// chain: 链式调用（从左到右）
let constraint chain2: any = constraint x: any => constraint f: any => constraint g: any => g(f(x));

let constraint chain3: any = constraint x: any => constraint f: any => constraint g: any => constraint h: any =>
    h(g(f(x)));

let constraint chain4: any = constraint x: any => constraint f: any => constraint g: any => constraint h: any => constraint i: any =>
    i(h(g(f(x))));

// ============ 重复应用组合子 ============

// repeat_apply: 重复应用函数 n 次
let constraint repeat_apply: any = constraint n: nat => constraint f: any => constraint x: any => {
    loop go: constraint (i: nat, acc: any) = (0, x);
    match i
        | assert n => acc
        | constraint _T: any => go(i + 1, f(acc))
        | panic
};

// ============ 比较组合子 ============

// equal: 相等比较
let constraint equal: any = constraint x: any => constraint y: any => x == y;

// not_equal: 不等比较
let constraint not_equal: any = constraint x: any => constraint y: any => not(x == y);

// greater_than: 大于
let constraint greater_than: any = constraint x: nat => constraint y: nat => x > y;

// less_than: 小于
let constraint less_than: any = constraint x: nat => constraint y: nat => x < y;

// greater_equal: 大于等于
let constraint greater_equal: any = constraint x: nat => constraint y: nat => x >= y;

// less_equal: 小于等于
let constraint less_equal: any = constraint x: nat => constraint y: nat => x <= y;

// ============ 导出所有组合子 ============

id::id &
const::const &
s::s &
compose::compose &
flip::flip &
dup::dup &
apply::apply &
pipe::pipe &
apply_twice::apply_twice &
not::not &
and::and &
or::or &
if_then_else::if_then_else &
when::when &
unless::unless &
compose3::compose3 &
compose4::compose4 &
on::on &
fix::fix &
inc::inc &
dec::dec &
double::double &
square::square &
ignore_first::ignore_first &
ignore_second::ignore_second &
void::void &
fst::fst &
snd::snd &
swap::swap &
both::both &
first::first &
second::second &
chain2::chain2 &
chain3::chain3 &
chain4::chain4 &
repeat_apply::repeat_apply &
equal::equal &
not_equal::not_equal &
greater_than::greater_than &
less_than::less_than &
greater_equal::greater_equal &
less_equal::less_equal
