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
let a: int = Positive 5;
let b: int = Negative 3;
let c: int = 0;
a + b, a - b, a * b, a / b, a % b, -a, a < b, a > b, a >= b, a <= b,
a + c, b - c, c * a, c / a, c % a, -c, c < a, c > a, c >= a, c <= a