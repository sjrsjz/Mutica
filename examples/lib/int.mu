let constraint throw_panic::(throw_panic: lambda) = import "panic.mu";
let constraint int: any = 0 | Positive::nat | Negative::nat;
extend $"op#add": match
    | assert (0, 0) => 0
    | constraint (Positive::(x: nat), 0) => Positive::x
    | constraint (Negative::(x: nat), 0) => Negative::x
    | constraint (0, Positive::(y: nat)) => Positive::y
    | constraint (0, Negative::(y: nat)) => Negative::y
    | constraint (Positive::(x: nat), Positive::(y: nat)) => Positive::(x + y)
    | constraint (Negative::(x: nat), Negative::(y: nat)) => Negative::(x + y)
    | constraint (Positive::(x: nat), Negative::(y: nat)) => if x > y then Positive::(x - y) else if x == y then 0 else Negative::(y - x)
    | constraint (Negative::(x: nat), Positive::(y: nat)) => if y > x then Positive::(y - x) else if x == y then 0 else Negative::(x - y)
    | panic;
extend $"op#sub": match
    | assert (0, 0) => 0
    | constraint (Positive::(x: nat), 0) => Positive::x
    | constraint (Negative::(x: nat), 0) => Negative::x
    | constraint (0, Positive::(y: nat)) => Negative::y
    | constraint (0, Negative::(y: nat)) => Positive::y
    | constraint (Positive::(x: nat), Positive::(y: nat)) => if x > y then Positive::(x - y) else if x == y then 0 else Negative::(y - x)
    | constraint (Negative::(x: nat), Negative::(y: nat)) => if x > y then Negative::(x - y) else if x == y then 0 else Positive::(y - x)
    | constraint (Positive::(x: nat), Negative::(y: nat)) => Positive::(x + y)
    | constraint (Negative::(x: nat), Positive::(y: nat)) => Negative::(x + y)
    | panic;
extend $"op#mul": match
    | assert (0, 0) => 0
    | constraint (Positive::nat, 0) => 0
    | constraint (Negative::nat, 0) => 0
    | constraint (0, Positive::nat) => 0
    | constraint (0, Negative::nat) => 0
    | constraint (Positive::(x: nat), Positive::(y: nat)) => Positive::(x * y)
    | constraint (Negative::(x: nat), Negative::(y: nat)) => Positive::(x * y)
    | constraint (Positive::(x: nat), Negative::(y: nat)) => Negative::(x * y)
    | constraint (Negative::(x: nat), Positive::(y: nat)) => Negative::(x * y)
    | panic;
extend $"op#div": match
    | constraint (Positive::(x: nat), Positive::(y: nat)) => {
        let constraint q: nat = x / y;
        if q == 0 then 0 else Positive::q
    }
    | constraint (Negative::(x: nat), Negative::(y: nat)) => {
        let constraint q: nat = x / y;
        if q == 0 then 0 else Positive::q
    }
    | constraint (Positive::(x: nat), Negative::(y: nat)) => {
        let constraint q: nat = x / y;
        if q == 0 then 0 else Negative::q
    }
    | constraint (Negative::(x: nat), Positive::(y: nat)) => {
        let constraint q: nat = x / y;
        if q == 0 then 0 else Negative::q
    }
    | assert (0, Positive::nat) => 0
    | assert (0, Negative::nat) => 0
    | panic;
extend $"op#mod": match
    | constraint (Positive::(x: nat), Positive::(y: nat)) => {
        let constraint r: nat = x % y;
        if r == 0 then 0 else Positive::r
    }
    | constraint (Negative::(x: nat), Negative::(y: nat)) => {
        let constraint r: nat = x % y;
        if r == 0 then 0 else Negative::r
    }
    | constraint (Positive::(x: nat), Negative::(y: nat)) => {
        let constraint r: nat = x % y;
        if r == 0 then 0 else Positive::r
    }
    | constraint (Negative::(x: nat), Positive::(y: nat)) => {
        let constraint r: nat = x % y;
        if r == 0 then 0 else Negative::r
    }
    | assert (0, Positive::nat) => 0
    | assert (0, Negative::nat) => 0
    | panic;
extend $"op#lt": match
    | assert (0, 0) => false
    | assert (Positive::nat, 0) => false
    | assert (Negative::nat, 0) => true
    | assert (0, Positive::nat) => true
    | assert (0, Negative::nat) => false
    | constraint (Positive::(x: nat), Positive::(y: nat)) => x < y
    | constraint (Negative::(x: nat), Negative::(y: nat)) => x > y
    | assert (Positive::nat, Negative::nat) => false
    | assert (Negative::nat, Positive::nat) => true
    | panic;
extend $"op#lte": match
    | assert (0, 0) => true
    | assert (Positive::nat, 0) => false
    | assert (Negative::nat, 0) => true
    | assert (0, Positive::nat) => true
    | assert (0, Negative::nat) => false
    | constraint (Positive::(x: nat), Positive::(y: nat)) => x <= y
    | constraint (Negative::(x: nat), Negative::(y: nat)) => x >= y
    | assert (Positive::nat, Negative::nat) => false
    | assert (Negative::nat, Positive::nat) => true
    | panic;
extend $"op#gt": match
    | assert (0, 0) => false
    | assert (Positive::nat, 0) => true
    | assert (Negative::nat, 0) => false
    | assert (0, Positive::nat) => false
    | assert (0, Negative::nat) => true
    | constraint (Positive::(x: nat), Positive::(y: nat)) => x > y
    | constraint (Negative::(x: nat), Negative::(y: nat)) => x < y
    | assert (Positive::nat, Negative::nat) => true
    | assert (Negative::nat, Positive::nat) => false
    | panic;
extend $"op#gte": match
    | assert (0, 0) => true
    | assert (Positive::nat, 0) => true
    | assert (Negative::nat, 0) => false
    | assert (0, Positive::nat) => false
    | assert (0, Negative::nat) => true
    | constraint (Positive::(x: nat), Positive::(y: nat)) => x >= y
    | constraint (Negative::(x: nat), Negative::(y: nat)) => x <= y
    | assert (Positive::nat, Negative::nat) => true
    | assert (Negative::nat, Positive::nat) => false
    | panic;
let constraint Positive: lambda = match
    | assert 0 => throw_panic "Cannot create Positive from 0"
    | constraint (x: nat) => Positive::x
    | panic;
let constraint Negative: lambda = match
    | assert 0 => throw_panic "Cannot create Negative from 0"
    | constraint (x: nat) => Negative::x
    | panic;
let constraint Zero: any = 0;
extend $"op#neg": match
    | assert 0 => 0
    | constraint Positive::(x: nat) => Negative::x
    | constraint Negative::(x: nat) => Positive::x
    | panic;

int::int & Zero::Zero & Positive::Positive & Negative::Negative & 
    Add::$"op#add" & Sub::$"op#sub" & Mul::$"op#mul" & Div::$"op#div" & Mod::$"op#mod" & 
    Lt::$"op#lt" & Gt::$"op#gt" & Lte::$"op#lte" & Gte::$"op#gte" & Neg::$"op#neg"