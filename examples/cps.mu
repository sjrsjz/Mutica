let program: any = {
    let f: any = next: any |-> x: int |-> {
        perform Print::'F'; // perform pattern = v; expr := v ~ (pattern -> expr)
                            // perform v; expr := perform _: any = v; expr
        let y: int = x + 1;
        next y
    };
    let g: any = next: any |-> x: int |-> {
        perform Print::'G';
        let y: int = x * 2;
        do f with y as v: int;
        next v
    };
    do g with 1 as v: int; // do f with v as pattern; expr := f(pattern -> expr)(v)
    v * 2
};
program