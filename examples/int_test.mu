let constraint {
    int::(int: any) &
    Positive::(Positive: lambda) &
    Negative::(Negative: lambda) &
    Add::($"op#add": lambda) &
    Sub::($"op#sub": lambda) &
    Mul::($"op#mul": lambda) &
    Div::($"op#div": lambda) &
    Mod::($"op#mod": lambda) &
    Lt::($"op#lt": lambda) &
    Gt::($"op#gt": lambda) &
    Lte::($"op#lte": lambda) &
    Gte::($"op#gte": lambda) &
    Neg::($"op#neg": lambda)
} = import "lib/int.mu";
let constraint a: int = Positive 5;
let constraint b: int = Negative 3;
let constraint c: int = 0;
a + b, a - b, a * b, a / b, a % b, -a, a < b, a > b, a >= b, a <= b,
a + c, b - c, c * a, c / a, c % a, -c, c < a, c > a, c >= a, c <= a