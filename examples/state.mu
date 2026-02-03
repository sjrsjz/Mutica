
let {
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = import "lib/string.mu";
let {
    bind::(bind: any) &
    return::(return: any) &
    eval::(eval: any)
} = import "lib/state.mu";

// --- 用户逻辑 ---

// tick: s -> k -> k(s, s+1)
let tick: any = s: nat => k: any => 
    k(s, s + 1);

let program: any = {
    (); // 封装作用域
    let next: any = bind;
    
    // 宏展开逻辑验证：
    // let v1 = #next tick; 
    // 变为: next(v1 => ...)(tick)
    // 这匹配 bind(f)(m) 的签名

    let print_a_add_b: any = (a: nat, b: nat) => {
        println("a: " + nat_to_string a);
        println("b: " + nat_to_string b);
        println("a + b: " + nat_to_string(a + b));
    };

    let v1: nat = #next tick;
    print_a_add_b(#next tick, #next tick);
    let (v2: nat, v3: nat) = (#next tick, #next tick);
    let v4: nat = #next tick;
    
    return (v1, v2, v3, v4)
};

let initial_state: nat = 10;

// 终点 Continuation
for (v1: nat, v2: nat, v3: nat, v4: nat) = program.eval(initial_state) in {
    println("Initial State: 10");
    println("v1: " + nat_to_string v1); // 10
    println("v2: " + nat_to_string v2); // 13
    println("v3: " + nat_to_string v3); // 14
    println("v4: " + nat_to_string v4); // 15
}
