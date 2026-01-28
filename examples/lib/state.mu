// return: a -> s -> k -> k((a, s))
let return: any = a: any => s: any => k: any => 
    k(a, s);

// bind: (f: Continuation) -> (m: Monad) -> (s: State) -> (k: FinalCont) -> ...
// 对应宏展开: next(v => rest)(tick)
// 所以第一个参数 f 是后续代码块 (v => rest)，第二个参数 m 是 tick
let bind: any = f: any => m: any => s: any => k: any => 
    m(s)((a: any, new_s: any) => 
            // f(a) 返回后续的 State Monad
            // 继续传入 new_s 和 k 驱动执行
            f a new_s k
    );

// eval: m -> s -> k -> ...
let eval: any = m: any => s: any => k: any =>
    m(s)((a: any, _s: any) => k a);

return::return &
bind::bind &
eval::eval