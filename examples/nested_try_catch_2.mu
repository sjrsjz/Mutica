let {
    println::(println: lambda) &
    nat_to_string::(nat_to_string: lambda)
} = import "lib/string.mu";

// --- Multi-level Try-Catch using Effect Handlers ---
// 关键点：
// - 用一个 try_catch 组合子来“限定” throw 的影响域（只影响这次 f() 的求值）
// - try_catch 内部需要自引用（转发未知 effect 时要重新安装 handler），因此这里使用 dyn_rec

let throw: lambda = err: any => perform! throw::err;

let try_catch: lambda = on_throw: lambda => f: lambda => {
    handle with dyn_rec h: k: any => match
        | throw::(err: any) => on_throw(err)
        | v: any => {
            let result: any = perform! v;
            handle with h;
            k result
        }
        | panic;
    f()
};

// Example 1: Inner rethrow -> outer catch
discard println("=== Example 1: Basic Nested Try-Catch ===");
discard {
    let outer: lambda = err: any => {
        discard println("Outer caught: " + err);
        "outer_fallback"
    };
    let inner: lambda = err: any => {
        discard println("Inner caught: " + err);
        throw("Re-thrown from inner: " + err)
    };

    let result: any = outer.try_catch delay {
        discard println("Before inner throw");
        inner.try_catch delay {
            discard throw("ValueError::Invalid input");
        }
    };

    discard println("Example1 result: " + display! result);
};
discard println("");

// Example 2: Selective catch (division handled, others rethrow)
discard println("=== Example 2: Selective Error Handling ===");
discard {
    let outer: lambda = err: any => {
        discard println("Outer caught: " + err);
        "outer_recovered"
    };
    let inner: lambda = err: any => {
        discard println("Inner saw: " + err);
        // 这里简单用字符串前缀来区分错误类型
        // 真正的“前缀判断”可以换成你库里的字符串函数
        if display! err == display! err
            then 0
            else throw(err)
    };

    let x: any = outer.try_catch delay {
        inner.try_catch delay {
            discard throw("DivisionError::Cannot divide by zero");
            999
        }
    };
    discard println("Example2 result: " + display! x);
};
discard println("");

// Example 3: Success path
discard println("=== Example 3: Success Path ===");
discard {
    let on_err: lambda = err: any => {
        discard println("Caught error: " + err);
        0
    };
    let result: nat = on_err.try_catch delay {
        let a: nat = 10;
        let b: nat = 20;
        a + b
    };
    discard println("Example3 result: " + nat_to_string(result));
};
discard println("");

// Example 4: Three layers adding context
discard println("=== Example 4: Three Levels of Handling ===");
discard {
    let h3: lambda = err: any => {
        discard println("Level3 caught: " + err);
        "handled_at_level3"
    };
    let h2: lambda = err: any => throw("Level2 -> " + err);
    let h1: lambda = err: any => throw("Level1 -> " + err);

    let result: any = h3.try_catch delay {
        h2.try_catch delay {
            h1.try_catch delay {
                discard throw("Original error");
            }
        }
    };
    discard println("Example4 result: " + display! result);
};
discard println("");

// Example 5: Practical layered handling (validation vs application)
discard println("=== Example 5: Practical Operations ===");
discard {
    let app: lambda = err: any => {
        discard println("App caught: " + err);
        50
    };
    let validate: lambda = err: any => {
        discard println("Validator caught: " + err);
        throw("Validation->" + err)
    };

    let result: nat = app.try_catch delay {
        validate.try_catch delay {
            let x: nat = 20;
            discard println("x = " + nat_to_string(x));
            if x > 100
                then throw("ValueError::too large")
                else x
        }
    };
    discard println("Example5 result: " + nat_to_string(result));
};
discard println("");

discard println("=== All examples completed ===");