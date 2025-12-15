let constraint int: any = 0 | Positive::[(), ..()] | Negative::[(), ..()];
extend $"op#add": match
    | constraint (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => Positive::(x + y)
    | constraint (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => Negative::(x + y)
    | constraint (Positive::(x: [(), ..()]), Negative::(y: [(), ..()])) => if x > y then Positive::(x - y) else if x == y then 0 else Negative::(y - x)
    | constraint (Negative::(x: [(), ..()]), Positive::(y: [(), ..()])) => if y > x then Positive::(y - x) else if x == y then 0 else Negative::(x - y)
    | constraint (Positive::(x: [(), ..()]), 0) => Positive::x
    | constraint (Negative::(x: [(), ..()]), 0) => Negative::x
    | constraint (0, Positive::(y: [(), ..()])) => Positive::y
    | constraint (0, Negative::(y: [(), ..()])) => Negative::y
    | assert (0, 0) => 0
    | panic;
extend $"op#sub": match
    | constraint (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => if x > y then Positive::(x - y) else if x == y then 0 else Negative::(y - x)
    | constraint (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => if x > y then Negative::(x - y) else if x == y then 0 else Positive::(y - x)
    | constraint (Positive::(x: [(), ..()]), Negative::(y: [(), ..()])) => Positive::(x + y)
    | constraint (Negative::(x: [(), ..()]), Positive::(y: [(), ..()])) => Negative::(x + y)
    | constraint (Positive::(x: [(), ..()]), 0) => Positive::x
    | constraint (Negative::(x: [(), ..()]), 0) => Negative::x
    | constraint (0, Positive::(y: [(), ..()])) => Negative::y
    | constraint (0, Negative::(y: [(), ..()])) => Positive::y
    | assert (0, 0) => 0
    | panic;
extend $"op#mul": match
    | constraint (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => Positive::(x * y)
    | constraint (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => Positive::(x * y)
    | constraint (Positive::(x: [(), ..()]), Negative::(y: [(), ..()])) => Negative::(x * y)
    | constraint (Negative::(x: [(), ..()]), Positive::(y: [(), ..()])) => Negative::(x * y)
    | constraint (Positive::[(), ..()], 0) => 0
    | constraint (Negative::[(), ..()], 0) => 0
    | constraint (0, Positive::[(), ..()]) => 0
    | constraint (0, Negative::[(), ..()]) => 0
    | assert (0, 0) => 0
    | panic;
extend $"op#div": match
    | constraint (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => {
        let constraint q: nat = x / y;
        if q == 0 then 0 else Positive::q
    }
    | constraint (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => {
        let constraint q: nat = x / y;
        if q == 0 then 0 else Positive::q
    }
    | constraint (Positive::(x: [(), ..()]), Negative::(y: [(), ..()])) => {
        let constraint q: nat = x / y;
        if q == 0 then 0 else Negative::q
    }
    | constraint (Negative::(x: [(), ..()]), Positive::(y: [(), ..()])) => {
        let constraint q: nat = x / y;
        if q == 0 then 0 else Negative::q
    }
    | assert (0, Positive::[(), ..()]) => 0
    | assert (0, Negative::[(), ..()]) => 0
    | panic;
extend $"op#mod": match
    | constraint (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => {
        let constraint r: nat = x % y;
        if r == 0 then 0 else Positive::r
    }
    | constraint (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => {
        let constraint r: nat = x % y;
        if r == 0 then 0 else Negative::r
    }
    | constraint (Positive::(x: [(), ..()]), Negative::(y: [(), ..()])) => {
        let constraint r: nat = x % y;
        if r == 0 then 0 else Positive::r
    }
    | constraint (Negative::(x: [(), ..()]), Positive::(y: [(), ..()])) => {
        let constraint r: nat = x % y;
        if r == 0 then 0 else Negative::r
    }
    | assert (0, Positive::[(), ..()]) => 0
    | assert (0, Negative::[(), ..()]) => 0
    | panic;
extend $"op#lt": match
    | constraint (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => x < y
    | constraint (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => x > y
    | assert (Positive::[(), ..()], Negative::[(), ..()]) => false
    | assert (Negative::[(), ..()], Positive::[(), ..()]) => true
    | assert (Positive::[(), ..()], 0) => false
    | assert (Negative::[(), ..()], 0) => true
    | assert (0, Positive::[(), ..()]) => true
    | assert (0, Negative::[(), ..()]) => false
    | assert (0, 0) => false
    | panic;
extend $"op#lte": match
    | constraint (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => x <= y
    | constraint (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => x >= y
    | assert (Positive::[(), ..()], Negative::[(), ..()]) => false
    | assert (Negative::[(), ..()], Positive::[(), ..()]) => true
    | assert (Positive::[(), ..()], 0) => false
    | assert (Negative::[(), ..()], 0) => true
    | assert (0, Positive::[(), ..()]) => true
    | assert (0, Negative::[(), ..()]) => false
    | assert (0, 0) => true
    | panic;
extend $"op#gt": match
    | constraint (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => x > y
    | constraint (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => x < y
    | assert (Positive::[(), ..()], Negative::[(), ..()]) => true
    | assert (Negative::[(), ..()], Positive::[(), ..()]) => false
    | assert (Positive::[(), ..()], 0) => true
    | assert (Negative::[(), ..()], 0) => false
    | assert (0, Positive::[(), ..()]) => false
    | assert (0, Negative::[(), ..()]) => true
    | assert (0, 0) => false
    | panic;
extend $"op#gte": match
    | constraint (Positive::(x: [(), ..()]), Positive::(y: [(), ..()])) => x >= y
    | constraint (Negative::(x: [(), ..()]), Negative::(y: [(), ..()])) => x <= y
    | assert (Positive::[(), ..()], Negative::[(), ..()]) => true
    | assert (Negative::[(), ..()], Positive::[(), ..()]) => false
    | assert (Positive::[(), ..()], 0) => true
    | assert (Negative::[(), ..()], 0) => false
    | assert (0, Positive::[(), ..()]) => false
    | assert (0, Negative::[(), ..()]) => true
    | assert (0, 0) => true
    | panic;
let constraint Positive: any = constraint (x: [(), ..()]) => Positive::x;
let constraint Negative: any = constraint (x: [(), ..()]) => Negative::x;
let constraint Zero: any = 0;
extend $"op#neg": match
    | constraint Positive::(x: [(), ..()]) => Negative::x
    | constraint Negative::(x: [(), ..()]) => Positive::x
    | assert 0 => 0
    | panic;

int::int & Zero::Zero & Positive::Positive & Negative::Negative & 
    Add::$"op#add" & Sub::$"op#sub" & Mul::$"op#mul" & Div::$"op#div" & Mod::$"op#mod" & 
    Lt::$"op#lt" & Gt::$"op#gt" & Lte::$"op#lte" & Gte::$"op#gte" & Neg::$"op#neg"