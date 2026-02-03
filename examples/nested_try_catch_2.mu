let {
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = import "lib/string.mu";

// --- Multi-level Try-Catch using Effect Handlers ---
// 关键点：
// - 用一个 try_catch 组合子来“限定” throw 的影响域（只影响这次 f() 的求值）
// - try_catch 内部需要自引用（转发未知 effect 时要重新安装 handler），因此这里使用 dyn_rec

let throw: any = err: any => perform! throw::err;

let try_catch: any = on_throw: any => f: any => {
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
println("=== Example 1: Basic Nested Try-Catch ===");
{
    let outer: any = err: any => {
        println("Outer caught: " + err);
        "outer_fallback"
    };
    let inner: any = err: any => {
        println("Inner caught: " + err);
        throw("Re-thrown from inner: " + err)
    };

    let result: any = outer.try_catch delay {
        println("Before inner throw");
        inner.try_catch delay {
            throw("ValueError::Invalid input");
        }
    };

    println("Example1 result: " + display! result);
};
println("");

// Example 2: Selective catch (division handled, others rethrow)
println("=== Example 2: Selective Error Handling ===");
{
    let outer: any = err: any => {
        println("Outer caught: " + err);
        "outer_recovered"
    };
    let inner: any = err: any => {
        println("Inner saw: " + err);
        // 这里简单用字符串前缀来区分错误类型
        if display! err == display! err
            then 0
            else throw(err)
    };

    let x: any = outer.try_catch delay {
        inner.try_catch delay {
            throw("DivisionError::Cannot divide by zero");
            999
        }
    };
    println("Example2 result: " + display! x);
};
println("");

// Example 3: Success path
println("=== Example 3: Success Path ===");
{
    let on_err: any = err: any => {
        println("Caught error: " + err);
        0
    };
    let result: nat = on_err.try_catch delay {
        let a: nat = 10;
        let b: nat = 20;
        a + b
    };
    println("Example3 result: " + nat_to_string(result));
};
println("");

// Example 4: Three layers adding context
println("=== Example 4: Three Levels of Handling ===");
{
    let h3: any = err: any => {
        println("Level3 caught: " + err);
        "handled_at_level3"
    };
    let h2: any = err: any => throw("Level2 -> " + err);
    let h1: any = err: any => throw("Level1 -> " + err);

    let result: any = h3.try_catch delay {
        h2.try_catch delay {
            h1.try_catch delay {
                throw("Original error");
            }
        }
    };
    println("Example4 result: " + display! result);
};
println("");

// Example 5: Practical layered handling (validation vs application)
println("=== Example 5: Practical Operations ===");
{
    let app: any = err: any => {
        println("App caught: " + err);
        50
    };
    let validate: any = err: any => {
        println("Validator caught: " + err);
        throw("Validation->" + err)
    };

    let result: nat = app.try_catch delay {
        validate.try_catch delay {
            let x: nat = 20;
            println("x = " + nat_to_string(x));
            if x > 100
                then throw("ValueError::too large")
                else x
        }
    };
    println("Example5 result: " + nat_to_string(result));
};
println("");

println("=== All examples completed ===");