{
    let constraint {
        println::(println: any) &
        nat_to_string::(nat_to_string: any)
    } = import "lib/string.mu";

    // --- 核心算子 ---

    // return: a -> s -> k -> k((a, s))
    let constraint return: any = constraint a: any => constraint s: any => constraint k: any => 
        k(a, s);

    // bind: (f: Continuation) -> (m: Monad) -> (s: State) -> (k: FinalCont) -> ...
    // 对应宏展开: next(v => rest)(tick)
    // 所以第一个参数 f 是后续代码块 (v => rest)，第二个参数 m 是 tick
    let constraint bind: any = constraint f: any => constraint m: any => constraint s: any => constraint k: any => 
        m(s)(constraint (a: any, new_s: any) => 
                // f(a) 返回后续的 State Monad
                // 继续传入 new_s 和 k 驱动执行
                f a new_s k
        );

    // eval: m -> s -> k -> ...
    let constraint eval: any = constraint m: any => constraint s: any => constraint k: any =>
        m(s)(constraint (a: any, _s: any) => k a);

    // --- 用户逻辑 ---

    // tick: s -> k -> k((s, s+1))
    let constraint tick: any = constraint s: nat => constraint k: any => 
        k(s, s + 1);

    let constraint program: any = {
        let constraint next: any = bind;
        
        // 宏展开逻辑验证：
        // let v1 = #next tick; 
        // 变为: next(constraint v1 => ...)(tick)
        // 这匹配 bind(f)(m) 的签名

        let constraint print_a_add_b: any = constraint (a: nat, b: nat) => {
            discard println("a: " + nat_to_string a);
            discard println("b: " + nat_to_string b);
            discard println("a + b: " + nat_to_string(a + b));
        };

        let constraint v1: nat = #next tick;
        discard print_a_add_b(#next tick, #next tick);
        let constraint (v2: nat, v3: nat) = (#next tick, #next tick);
        let constraint v4: nat = #next tick;
        
        return (v1, v2, v3, v4)
    };

    let constraint initial_state: nat = 10;
    
    // 终点 Continuation
    let constraint main_continuation: any = constraint result: (nat, nat, nat, nat) => {
        discard println("Initial State: 10");
        let constraint (v1: nat, v2: nat, v3: nat, v4: nat) = result;
        discard println("v1: " + nat_to_string v1); // 10
        discard println("v2: " + nat_to_string v2); // 11
        discard println("v3: " + nat_to_string v3); // 12
        discard println("v4: " + nat_to_string v4); // 13
    };

    // --- 执行 ---
    // 整个表达式构建了一个完整的 Invoke 链，并作为返回值交给 VM
    eval program initial_state main_continuation
}