let f: any = () |-> ();
#f();   // discard the return value, alias `let _: () = f();`.
        // Note that `f()` must return `()`, otherwise it is a type error.
discard f();    // same as above