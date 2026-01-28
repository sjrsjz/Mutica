let deref::($"op#not": any) = import "lib/mutable.mu";
let {
    int::(int: any) &
    Positive::(Positive: any) &
    Negative::(Negative: any) &
    Add::($"op#add": any) &
    Sub::($"op#sub": any) &
    Mul::($"op#mul": any) &
    Div::($"op#div": any) &
    Mod::($"op#mod": any) &
    Lt::($"op#lt": any) &
    Gt::($"op#gt": any) &
    Lte::($"op#lte": any) &
    Gte::($"op#gte": any) &
    Neg::($"op#neg": any)
} = import "lib/int.mu";

let A: any = dyn_rec A: (k: int, x1: any, x2: any, x3: any, x4: any, x5: any) => {
    let k: any = mut k;
    let B: any = dyn_rec B: delay {
        discard k := !k - Positive 1;
        A(!k, B, x1, x2, x3, x4)
    };
    if !k <= 0 then x4() + x5() else B()
};
A(Positive 10, delay (Positive 1), delay (Negative 1), delay (Negative 1), delay (Positive 1), delay 0)