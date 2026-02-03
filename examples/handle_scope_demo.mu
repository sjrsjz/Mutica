let {
    println::(println: any)
} = import "lib/string.mu";

let throw: any = err: any => perform! throw::err;

// 一个最小的 try/catch handler：只截获 throw::err，其它 effect 透传
let try_catch: any = on_throw: any => f: any => {
    handle with dyn_rec h: k: any => match
        | throw::(err: any) => on_throw(err)
        | v: any => {
            let r: any = perform! v;
            handle with h;
            k r
        }
        | panic;
    f()
};

// --------------------------------------------------
// Case A：错误示范：handle with 吃掉“后面整段表达式”作为 <expr>
// --------------------------------------------------

println("=== Case A: handler wraps the rest ===");

{
    handle with dyn_rec h: k: any => match
        | throw::(err: any) => {
            println("[A] caught: " + err);
        }
        | v: any => {
            let r: any = perform! v;
            handle with h;
            k r
        }
        | panic;

    println("[A] before throw");
    throw("boom");
    println("[A] after throw (WON'T RUN)");
    println("[A] after handler (WON'T RUN)");
    ()
};

println("[A] after calling case_a (WILL RUN)");

println("");

// --------------------------------------------------
// Case B：正确示范：用一个“显式子表达式”限制 handler 的 <expr>
// 让 throw 只短路子表达式，后续还能继续
// --------------------------------------------------

println("=== Case B: limit <expr> with braces ===");

// 这里 try_catch 的作用域只覆盖这个 delay{...}
// 所以不会把 Case C 也一起吃掉
let _b: any = try_catch(err: any => {
    println("[B] caught: " + err);
    "b_recovered"
})(delay {
    println("[B] before throw");
    throw("boom");
    println("[B] after throw (WON'T RUN)");
    "b_ok"
});

println("[B] after try_catch (WILL RUN)");

println("");

// --------------------------------------------------
// Case C：同样的道理：只要用分号把边界切开
// handler 即使不调用 continuation，也只会短路它包住的那段 <expr>
// --------------------------------------------------

println("=== Case C: { handle with ...; <expr> } ; next ===");

{
    handle with dyn_rec h: k: any => match
        | throw::(err: any) => {
            println("[C] caught: " + err);
        }
        | v: any => {
            let r: any = perform! v;
            handle with h;
            k r
        }
        | panic;

    println("[C] before throw");
    throw("boom");
    println("[C] after throw (WON'T RUN)");
};

println("[C] after discard-block (WILL RUN)");