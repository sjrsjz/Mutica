let int: any = 0 | Positive::[(), ..()] | Negative::[(), ..()];
extend $"op#add": match
    | (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => Positive::(x + y)
    | (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => Negative::(x + y)
    | (Positive::(x: [(), ..()]), Negative::(y: [(), ..()])) => if x > y then Positive::(x - y) else if x == y then 0 else Negative::(y - x)
    | (Negative::(x: [(), ..()]), Positive::(y: [(), ..()])) => if y > x then Positive::(y - x) else if x == y then 0 else Negative::(x - y)
    | (Positive::(x: [(), ..()]), 0) => Positive::x
    | (Negative::(x: [(), ..()]), 0) => Negative::x
    | (0, Positive::(y: [(), ..()])) => Positive::y
    | (0, Negative::(y: [(), ..()])) => Negative::y
    | (0, 0) => 0
    | panic;
extend $"op#sub": match
    | (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => if x > y then Positive::(x - y) else if x == y then 0 else Negative::(y - x)
    | (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => if x > y then Negative::(x - y) else if x == y then 0 else Positive::(y - x)
    | (Positive::(x: [(), ..()]), Negative::(y: [(), ..()])) => Positive::(x + y)
    | (Negative::(x: [(), ..()]), Positive::(y: [(), ..()])) => Negative::(x + y)
    | (Positive::(x: [(), ..()]), 0) => Positive::x
    | (Negative::(x: [(), ..()]), 0) => Negative::x
    | (0, Positive::(y: [(), ..()])) => Negative::y
    | (0, Negative::(y: [(), ..()])) => Positive::y
    | (0, 0) => 0
    | panic;
extend $"op#mul": match
    | (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => Positive::(x * y)
    | (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => Positive::(x * y)
    | (Positive::(x: [(), ..()]), Negative::(y: [(), ..()])) => Negative::(x * y)
    | (Negative::(x: [(), ..()]), Positive::(y: [(), ..()])) => Negative::(x * y)
    | (Positive::[(), ..()], 0) => 0
    | (Negative::[(), ..()], 0) => 0
    | (0, Positive::[(), ..()]) => 0
    | (0, Negative::[(), ..()]) => 0
    | (0, 0) => 0
    | panic;
extend $"op#div": match
    | (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => {
        let q: nat = x / y;
        if q == 0 then 0 else Positive::q
    }
    | (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => {
        let q: nat = x / y;
        if q == 0 then 0 else Positive::q
    }
    | (Positive::(x: [(), ..()]), Negative::(y: [(), ..()])) => {
        let q: nat = x / y;
        if q == 0 then 0 else Negative::q
    }
    | (Negative::(x: [(), ..()]), Positive::(y: [(), ..()])) => {
        let q: nat = x / y;
        if q == 0 then 0 else Negative::q
    }
    | (0, Positive::[(), ..()]) => 0
    | (0, Negative::[(), ..()]) => 0
    | panic;
extend $"op#mod": match
    | (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => {
        let r: nat = x % y;
        if r == 0 then 0 else Positive::r
    }
    | (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => {
        let r: nat = x % y;
        if r == 0 then 0 else Negative::r
    }
    | (Positive::(x: [(), ..()]), Negative::(y: [(), ..()])) => {
        let r: nat = x % y;
        if r == 0 then 0 else Positive::r
    }
    | (Negative::(x: [(), ..()]), Positive::(y: [(), ..()])) => {
        let r: nat = x % y;
        if r == 0 then 0 else Negative::r
    }
    | (0, Positive::[(), ..()]) => 0
    | (0, Negative::[(), ..()]) => 0
    | panic;
extend $"op#lt": match
    | (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => x < y
    | (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => x > y
    | (Positive::[(), ..()], Negative::[(), ..()]) => false
    | (Negative::[(), ..()], Positive::[(), ..()]) => true
    | (Positive::[(), ..()], 0) => false
    | (Negative::[(), ..()], 0) => true
    | (0, Positive::[(), ..()]) => true
    | (0, Negative::[(), ..()]) => false
    | (0, 0) => false
    | panic;
extend $"op#lte": match
    | (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => x <= y
    | (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => x >= y
    | (Positive::[(), ..()], Negative::[(), ..()]) => false
    | (Negative::[(), ..()], Positive::[(), ..()]) => true
    | (Positive::[(), ..()], 0) => false
    | (Negative::[(), ..()], 0) => true
    | (0, Positive::[(), ..()]) => true
    | (0, Negative::[(), ..()]) => false
    | (0, 0) => true
    | panic;
extend $"op#gt": match
    | (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => x > y
    | (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => x < y
    | (Positive::[(), ..()], Negative::[(), ..()]) => true
    | (Negative::[(), ..()], Positive::[(), ..()]) => false
    | (Positive::[(), ..()], 0) => true
    | (Negative::[(), ..()], 0) => false
    | (0, Positive::[(), ..()]) => false
    | (0, Negative::[(), ..()]) => true
    | (0, 0) => false
    | panic;
extend $"op#gte": match
    | (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => x >= y
    | (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => x <= y
    | (Positive::[(), ..()], Negative::[(), ..()]) => true
    | (Negative::[(), ..()], Positive::[(), ..()]) => false
    | (Positive::[(), ..()], 0) => true
    | (Negative::[(), ..()], 0) => false
    | (0, Positive::[(), ..()]) => false
    | (0, Negative::[(), ..()]) => true
    | (0, 0) => true
    | panic;
let Positive: any = (x: [(), ..()]) => Positive::x;
let Negative: any = (x: [(), ..()]) => Negative::x;
let Zero: any = 0;
extend $"op#neg": match
    | Positive::(x: [(), ..()]) => Negative::x
    | Negative::(x: [(), ..()]) => Positive::x
    | 0 => 0
    | panic;

int::int & Zero::Zero & Positive::Positive & Negative::Negative & 
    Add::$"op#add" & Sub::$"op#sub" & Mul::$"op#mul" & Div::$"op#div" & Mod::$"op#mod" & 
    Lt::$"op#lt" & Gt::$"op#gt" & Lte::$"op#lte" & Gte::$"op#gte" & Neg::$"op#neg"