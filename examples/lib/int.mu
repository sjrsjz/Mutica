let int: any = 0 | Positive::1..! | Negative::1..!;
extend $"op#add": match
    | (Positive::(x: 1..!), Positive::(y: 1..!)) => Positive::(x + y)
    | (Negative::(x: 1..!), Negative::(y: 1..!)) => Negative::(x + y)
    | (Positive::(x: 1..!), Negative::(y: 1..!)) => if x > y then Positive::(x - y) else if x == y then 0 else Negative::(y - x)
    | (Negative::(x: 1..!), Positive::(y: 1..!)) => if y > x then Positive::(y - x) else if x == y then 0 else Negative::(x - y)
    | (Positive::(x: 1..!), 0) => Positive::x
    | (Negative::(x: 1..!), 0) => Negative::x
    | (0, Positive::(y: 1..!)) => Positive::y
    | (0, Negative::(y: 1..!)) => Negative::y
    | (0, 0) => 0
    | panic;
extend $"op#sub": match
    | (Positive::(x: 1..!), Positive::(y: 1..!)) => if x > y then Positive::(x - y) else if x == y then 0 else Negative::(y - x)
    | (Negative::(x: 1..!), Negative::(y: 1..!)) => if x > y then Negative::(x - y) else if x == y then 0 else Positive::(y - x)
    | (Positive::(x: 1..!), Negative::(y: 1..!)) => Positive::(x + y)
    | (Negative::(x: 1..!), Positive::(y: 1..!)) => Negative::(x + y)
    | (Positive::(x: 1..!), 0) => Positive::x
    | (Negative::(x: 1..!), 0) => Negative::x
    | (0, Positive::(y: 1..!)) => Negative::y
    | (0, Negative::(y: 1..!)) => Positive::y
    | (0, 0) => 0
    | panic;
extend $"op#mul": match
    | (Positive::(x: 1..!), Positive::(y: 1..!)) => Positive::(x * y)
    | (Negative::(x: 1..!), Negative::(y: 1..!)) => Positive::(x * y)
    | (Positive::(x: 1..!), Negative::(y: 1..!)) => Negative::(x * y)
    | (Negative::(x: 1..!), Positive::(y: 1..!)) => Negative::(x * y)
    | (Positive::1..!, 0) => 0
    | (Negative::1..!, 0) => 0
    | (0, Positive::1..!) => 0
    | (0, Negative::1..!) => 0
    | (0, 0) => 0
    | panic;
extend $"op#div": match
    | (Positive::(x: 1..!), Positive::(y: 1..!)) => {
        let q: nat = x / y;
        if q == 0 then 0 else Positive::q
    }
    | (Negative::(x: 1..!), Negative::(y: 1..!)) => {
        let q: nat = x / y;
        if q == 0 then 0 else Positive::q
    }
    | (Positive::(x: 1..!), Negative::(y: 1..!)) => {
        let q: nat = x / y;
        if q == 0 then 0 else Negative::q
    }
    | (Negative::(x: 1..!), Positive::(y: 1..!)) => {
        let q: nat = x / y;
        if q == 0 then 0 else Negative::q
    }
    | (0, Positive::1..!) => 0
    | (0, Negative::1..!) => 0
    | panic;
extend $"op#mod": match
    | (Positive::(x: 1..!), Positive::(y: 1..!)) => {
        let r: nat = x % y;
        if r == 0 then 0 else Positive::r
    }
    | (Negative::(x: 1..!), Negative::(y: 1..!)) => {
        let r: nat = x % y;
        if r == 0 then 0 else Negative::r
    }
    | (Positive::(x: 1..!), Negative::(y: 1..!)) => {
        let r: nat = x % y;
        if r == 0 then 0 else Positive::r
    }
    | (Negative::(x: 1..!), Positive::(y: 1..!)) => {
        let r: nat = x % y;
        if r == 0 then 0 else Negative::r
    }
    | (0, Positive::1..!) => 0
    | (0, Negative::1..!) => 0
    | panic;
extend $"op#lt": match
    | (Positive::(x: 1..!), Positive::(y: 1..!)) => x < y
    | (Negative::(x: 1..!), Negative::(y: 1..!)) => x > y
    | (Positive::1..!, Negative::1..!) => false
    | (Negative::1..!, Positive::1..!) => true
    | (Positive::1..!, 0) => false
    | (Negative::1..!, 0) => true
    | (0, Positive::1..!) => true
    | (0, Negative::1..!) => false
    | (0, 0) => false
    | panic;
extend $"op#lte": match
    | (Positive::(x: 1..!), Positive::(y: 1..!)) => x <= y
    | (Negative::(x: 1..!), Negative::(y: 1..!)) => x >= y
    | (Positive::1..!, Negative::1..!) => false
    | (Negative::1..!, Positive::1..!) => true
    | (Positive::1..!, 0) => false
    | (Negative::1..!, 0) => true
    | (0, Positive::1..!) => true
    | (0, Negative::1..!) => false
    | (0, 0) => true
    | panic;
extend $"op#gt": match
    | (Positive::(x: 1..!), Positive::(y: 1..!)) => x > y
    | (Negative::(x: 1..!), Negative::(y: 1..!)) => x < y
    | (Positive::1..!, Negative::1..!) => true
    | (Negative::1..!, Positive::1..!) => false
    | (Positive::1..!, 0) => true
    | (Negative::1..!, 0) => false
    | (0, Positive::1..!) => false
    | (0, Negative::1..!) => true
    | (0, 0) => false
    | panic;
extend $"op#gte": match
    | (Positive::(x: 1..!), Positive::(y: 1..!)) => x >= y
    | (Negative::(x: 1..!), Negative::(y: 1..!)) => x <= y
    | (Positive::1..!, Negative::1..!) => true
    | (Negative::1..!, Positive::1..!) => false
    | (Positive::1..!, 0) => true
    | (Negative::1..!, 0) => false
    | (0, Positive::1..!) => false
    | (0, Negative::1..!) => true
    | (0, 0) => true
    | panic;
let Positive: any = (x: 1..!) => Positive::x;
let Negative: any = (x: 1..!) => Negative::x;
let Zero: any = 0;
extend $"op#neg": match
    | Positive::(x: 1..!) => Negative::x
    | Negative::(x: 1..!) => Positive::x
    | 0 => 0
    | panic;

int::int & Zero::Zero & Positive::Positive & Negative::Negative & 
    Add::$"op#add" & Sub::$"op#sub" & Mul::$"op#mul" & Div::$"op#div" & Mod::$"op#mod" & 
    Lt::$"op#lt" & Gt::$"op#gt" & Lte::$"op#lte" & Gte::$"op#gte" & Neg::$"op#neg"