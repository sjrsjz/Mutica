// Combinator Library - 组合子库
// 包含纯理论性质的经典组合子

// ============ SKI 组合子系统 ============

// I combinator (Identity) - 恒等函数
let id: any = x: any => x;

// K combinator (Constant) - 常量函数
let const: any = x: any => _y: any => x;

// S combinator - 组合应用
let s: any = f: any => g: any => x: any => f(x)(g(x));

// ============ 其他经典组合子 ============

// B combinator (Compose) - 函数组合
let compose: any = f: any => g: any => x: any => f(g(x));

// C combinator (Flip) - 翻转参数
let flip: any = f: any => x: any => y: any => f(y)(x);

// W combinator (Duplication) - 参数复制
let dup: any = f: any => x: any => f(x)(x);

// Y combinator (Fixed-point) - 不动点组合子
let fix: any = f: any => {
    let go: any = dyn_rec go: f(go);
    go
};

// ============ 元组投影组合子 ============

// fst: 获取二元组第一个元素
let fst: any = (x: any, _y: any) => x;

// snd: 获取二元组第二个元素
let snd: any = (_x: any, y: any) => y;

// ============ 导出所有组合子 ============

id::id &
const::const &
s::s &
compose::compose &
flip::flip &
dup::dup &
fix::fix &
fst::fst &
snd::snd
