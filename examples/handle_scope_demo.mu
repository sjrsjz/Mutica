let {
    println::(println: lambda)
} = import "lib/string.mu";

let throw: lambda = err: any => perform! throw::err;

// 一个最小的 try/catch handler：只截获 throw::err，其它 effect 透传
let try_catch: lambda = on_throw: lambda => f: lambda => {
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

discard println("=== Case A: handler wraps the rest ===");

discard {
    handle with dyn_rec h: k: any => match
        | throw::(err: any) => {
            discard println("[A] caught: " + err);
        }
        | v: any => {
            let r: any = perform! v;
            handle with h;
            k r
        }
        | panic;

    discard println("[A] before throw");
    discard throw("boom");
    discard println("[A] after throw (WON'T RUN)");
    discard println("[A] after handler (WON'T RUN)");
    ()
};

discard println("[A] after calling case_a (WILL RUN)");

discard println("");

// --------------------------------------------------
// Case B：正确示范：用一个“显式子表达式”限制 handler 的 <expr>
// 让 throw 只短路子表达式，后续还能继续
// --------------------------------------------------

discard println("=== Case B: limit <expr> with braces ===");

// 这里 try_catch 的作用域只覆盖这个 delay{...}
// 所以不会把 Case C 也一起吃掉
let _b: any = try_catch(err: any => {
    discard println("[B] caught: " + err);
    "b_recovered"
})(delay {
    discard println("[B] before throw");
    discard throw("boom");
    discard println("[B] after throw (WON'T RUN)");
    "b_ok"
});

discard println("[B] after try_catch (WILL RUN)");

discard println("");

// --------------------------------------------------
// Case C：同样的道理：只要用分号把边界切开
// handler 即使不调用 continuation，也只会短路它包住的那段 <expr>
// --------------------------------------------------

discard println("=== Case C: discard { handle with ...; <expr> } ; next ===");

discard {
    handle with dyn_rec h: k: any => match
        | throw::(err: any) => {
            discard println("[C] caught: " + err);
        }
        | v: any => {
            let r: any = perform! v;
            handle with h;
            k r
        }
        | panic;

    discard println("[C] before throw");
    discard throw("boom");
    discard println("[C] after throw (WON'T RUN)");
};

discard println("[C] after discard-block (WILL RUN)");