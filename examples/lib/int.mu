let throw_panic::(throw_panic: (lambda | panic)) = import "panic.mu";
let int: any = 0 | Positive::nat | Negative::nat;
extend $"op#add": match
    | (0, 0) => 0
    | (Positive::(x: nat), 0) => Positive::x
    | (Negative::(x: nat), 0) => Negative::x
    | (0, Positive::(y: nat)) => Positive::y
    | (0, Negative::(y: nat)) => Negative::y
    | (Positive::(x: nat), Positive::(y: nat)) => Positive::(x + y)
    | (Negative::(x: nat), Negative::(y: nat)) => Negative::(x + y)
    | (Positive::(x: nat), Negative::(y: nat)) => if x > y then Positive::(x - y) else if x == y then 0 else Negative::(y - x)
    | (Negative::(x: nat), Positive::(y: nat)) => if y > x then Positive::(y - x) else if x == y then 0 else Negative::(x - y)
    | panic;
extend $"op#sub": match
    | (0, 0) => 0
    | (Positive::(x: nat), 0) => Positive::x
    | (Negative::(x: nat), 0) => Negative::x
    | (0, Positive::(y: nat)) => Negative::y
    | (0, Negative::(y: nat)) => Positive::y
    | (Positive::(x: nat), Positive::(y: nat)) => if x > y then Positive::(x - y) else if x == y then 0 else Negative::(y - x)
    | (Negative::(x: nat), Negative::(y: nat)) => if x > y then Negative::(x - y) else if x == y then 0 else Positive::(y - x)
    | (Positive::(x: nat), Negative::(y: nat)) => Positive::(x + y)
    | (Negative::(x: nat), Positive::(y: nat)) => Negative::(x + y)
    | panic;
extend $"op#mul": match
    | (0, 0) => 0
    | (Positive::nat, 0) => 0
    | (Negative::nat, 0) => 0
    | (0, Positive::nat) => 0
    | (0, Negative::nat) => 0
    | (Positive::(x: nat), Positive::(y: nat)) => Positive::(x * y)
    | (Negative::(x: nat), Negative::(y: nat)) => Positive::(x * y)
    | (Positive::(x: nat), Negative::(y: nat)) => Negative::(x * y)
    | (Negative::(x: nat), Positive::(y: nat)) => Negative::(x * y)
    | panic;
extend $"op#div": match
    | (Positive::(x: nat), Positive::(y: nat)) => {
        let q: nat = x / y;
        if q == 0 then 0 else Positive::q
    }
    | (Negative::(x: nat), Negative::(y: nat)) => {
        let q: nat = x / y;
        if q == 0 then 0 else Positive::q
    }
    | (Positive::(x: nat), Negative::(y: nat)) => {
        let q: nat = x / y;
        if q == 0 then 0 else Negative::q
    }
    | (Negative::(x: nat), Positive::(y: nat)) => {
        let q: nat = x / y;
        if q == 0 then 0 else Negative::q
    }
    | (0, Positive::nat) => 0
    | (0, Negative::nat) => 0
    | panic;
extend $"op#mod": match
    | (Positive::(x: nat), Positive::(y: nat)) => {
        let r: nat = x % y;
        if r == 0 then 0 else Positive::r
    }
    | (Negative::(x: nat), Negative::(y: nat)) => {
        let r: nat = x % y;
        if r == 0 then 0 else Negative::r
    }
    | (Positive::(x: nat), Negative::(y: nat)) => {
        let r: nat = x % y;
        if r == 0 then 0 else Positive::r
    }
    | (Negative::(x: nat), Positive::(y: nat)) => {
        let r: nat = x % y;
        if r == 0 then 0 else Negative::r
    }
    | (0, Positive::nat) => 0
    | (0, Negative::nat) => 0
    | panic;
extend $"op#lt": match
    | (0, 0) => false
    | (Positive::nat, 0) => false
    | (Negative::nat, 0) => true
    | (0, Positive::nat) => true
    | (0, Negative::nat) => false
    | (Positive::(x: nat), Positive::(y: nat)) => x < y
    | (Negative::(x: nat), Negative::(y: nat)) => x > y
    | (Positive::nat, Negative::nat) => false
    | (Negative::nat, Positive::nat) => true
    | panic;
extend $"op#lte": match
    | (0, 0) => true
    | (Positive::nat, 0) => false
    | (Negative::nat, 0) => true
    | (0, Positive::nat) => true
    | (0, Negative::nat) => false
    | (Positive::(x: nat), Positive::(y: nat)) => x <= y
    | (Negative::(x: nat), Negative::(y: nat)) => x >= y
    | (Positive::nat, Negative::nat) => false
    | (Negative::nat, Positive::nat) => true
    | panic;
extend $"op#gt": match
    | (0, 0) => false
    | (Positive::nat, 0) => true
    | (Negative::nat, 0) => false
    | (0, Positive::nat) => false
    | (0, Negative::nat) => true
    | (Positive::(x: nat), Positive::(y: nat)) => x > y
    | (Negative::(x: nat), Negative::(y: nat)) => x < y
    | (Positive::nat, Negative::nat) => true
    | (Negative::nat, Positive::nat) => false
    | panic;
extend $"op#gte": match
    | (0, 0) => true
    | (Positive::nat, 0) => true
    | (Negative::nat, 0) => false
    | (0, Positive::nat) => false
    | (0, Negative::nat) => true
    | (Positive::(x: nat), Positive::(y: nat)) => x >= y
    | (Negative::(x: nat), Negative::(y: nat)) => x <= y
    | (Positive::nat, Negative::nat) => true
    | (Negative::nat, Positive::nat) => false
    | panic;
let Positive: any = match
    | 0 => throw_panic "Cannot create Positive from 0"
    | (x: nat) => Positive::x
    | panic;
let Negative: any = match
    | 0 => throw_panic "Cannot create Negative from 0"
    | (x: nat) => Negative::x
    | panic;
let Zero: any = 0;
extend $"op#neg": match
    | 0 => 0
    | Positive::(x: nat) => Negative::x
    | Negative::(x: nat) => Positive::x
    | panic;

int::int & Zero::Zero & Positive::Positive & Negative::Negative & 
    Add::$"op#add" & Sub::$"op#sub" & Mul::$"op#mul" & Div::$"op#div" & Mod::$"op#mod" & 
    Lt::$"op#lt" & Gt::$"op#gt" & Lte::$"op#lte" & Gte::$"op#gte" & Neg::$"op#neg"