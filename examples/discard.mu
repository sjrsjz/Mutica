let constraint f: any = assert () => ();
discard f();    // discard the return value, alias `let constraint _: () = f();`.
                // Note that `f()` must return `()`, otherwise it is a type error.