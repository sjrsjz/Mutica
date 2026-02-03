let f: any = () => ();
f();    // the return value, alias `let _: () = f();`.
                // Note that `f()` must return `()`, otherwise it is a type error.