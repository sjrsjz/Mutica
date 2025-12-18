let constraint maybe_pkg: any = import "maybe.mu";
let constraint Any::(Any: any) = import "any.mu";
let constraint throw_panic::(throw_panic: any) = import "panic.mu";

let constraint List: any = constraint T: any => rec list: (() | (T ~ list));

let constraint Greater: any = constraint (T: any, n: nat) => {
    let constraint go: any = dyn_rec go: match
        | assert 0 => List(T)
        | constraint m: nat => {
            if m > 0
                then (T ~ go(m - 1))
                else throw_panic("Invalid Greater: n must be >= 0")
        }
        | panic;
    go(n)
};

let constraint Range: any = constraint (T: any, min: nat, max: nat) => {
    let constraint go: any = dyn_rec go: match
        | assert (0, 0) => ()
        | constraint (0, m: nat) => {
            if m > 0
                then (() | (T ~ go(0, m - 1)))
                else throw_panic("Invalid Range: max must be > 0 in this branch")
        }
        | constraint (n: nat, m: nat) => {
            if n > 0 then {
                if m >= n
                    then (T ~ go(n - 1, m - 1))
                    else throw_panic("Invalid Range: max must be >= min")
            }
            else throw_panic("Invalid Range: min must be >= 0")
        }
        | panic;
    go(min, max)
};

let constraint Exact: any = constraint (T: any, n: nat) => {
    let constraint go: any = dyn_rec go: match
        | assert 0 => ()
        | constraint m: nat => {
            if m > 0
                then (T,) + go(m - 1)
                else throw_panic("Cannot create Exact with negative length")
        }
        | panic;
    go(n)
};

let constraint Modular: any = constraint (T: any, a: nat, b: nat) => {
    let constraint cycle: any = dyn_rec cycle: {
        let constraint add_a: any = dyn_rec add_a: constraint (count: nat, tail_type: any) => match count
            | assert 0 => tail_type
            | constraint c: nat => {
                if c > 0 
                    then (T ~ add_a((c - 1, tail_type)))
                    else throw_panic("Invalid Modular: a must be > 0")
            }
            | panic;
        (() | add_a((a, cycle)))
    };
    
    let constraint add_b: any = dyn_rec add_b: constraint (count: nat, tail_type: any) => match count
        | assert 0 => tail_type
        | constraint c: nat => {
            if c >= 0 
                then (T ~ add_b((c - 1, tail_type)))
                else throw_panic("Invalid Modular: b must be >= 0")
            }
        | panic;
    add_b((b, cycle))
};

let constraint Nil: any = ();
let constraint cons: any = constraint (head: any, tail: any) => (head,) + tail;
let constraint head: any = match
    | constraint (h: any ~ _T: _) => h
    | panic;
let constraint tail: any = match
    | constraint (_T: _ ~ t: any) => t
    | panic;
let constraint is_nil: any = match
    | assert () => true
    | constraint _T: _ => false
    | panic;
let constraint iter: any = constraint lst: List(Any) => constraint f: any => {
    loop go: constraint t: any = lst;
    match t
        | assert () => ()
        | constraint (h: any ~ t: any) => {
            discard f(h);
            go(t)
        }
        | panic
};
let constraint iteri: any = constraint lst: List(Any) => constraint f: any => {
    loop go: constraint (t: any, index: nat) = (lst, 0);
    match t
        | assert () => ()
        | constraint (h: any ~ t: any) => {
            discard f(index, h);
            go(t, index + 1)
        }
        | panic
};
let constraint map: any = constraint lst: List(Any) => constraint f: any => {
    loop go: constraint t: any = lst;
    match t
        | assert () => ()
        | constraint (h: any ~ t: any) => cons(f(h), go(t))
        | panic
};
let constraint len: any = constraint lst: List(Any) => {
    loop go: constraint t: any = lst;
    match t
        | assert () => 0
        | constraint (_T: _ ~ t: any) => 1 + go(t)
        | panic
};
let constraint filter: any = constraint lst: List(Any) => constraint pred: any => {
    loop go: constraint t: any = lst;
    match t
        | assert () => ()
        | constraint (h: any ~ t: any) => if pred(h)
            then cons(h, go(t))
            else go(t)
        | panic
};
let constraint fold: any = 
    constraint lst: List(Any) => 
    constraint acc: any => 
    constraint f: any => {
    loop go: constraint (t: any, a: any) = (lst, acc);
    match t
        | assert () => a
        | constraint (h: any ~ t: any) => go(t, f(a, h))
        | panic
};
let constraint foldr: any = 
    constraint lst: List(Any) => 
    constraint acc: any => 
    constraint f: any => {
    loop go: constraint t: any = lst;
    match t
        | assert () => acc
        | constraint (h: any ~ t: any) => f(h, go(t))
        | panic
};
let constraint append: any = 
    constraint lst1: List(Any) => 
    constraint lst2: List(Any) => {
    lst1 + lst2
};
let constraint reverse: any = constraint lst: List(Any) => {
    loop go: constraint t: any = (lst, ());
    match t
        | constraint ((), acc: any) => acc
        | constraint ((h: any ~ t: any), acc: any) => go(t, cons(h, acc))
        | panic
};
let constraint nth: any = constraint lst: List(Any) => constraint n: nat => {
    loop go: constraint (t: any, i: nat) = (lst, n);
    match (t, i)
        | constraint ((h: any ~ _T: _) , 0) => h
        | constraint ((_T: _ ~ t: any), i: nat) => go(t, i - 1)
        | panic
};
let constraint take: any = constraint lst: List(Any) => constraint n: nat => {
    loop go: constraint (t: any, i: nat) = (lst, n);
    match (t, i)
        | constraint ((), _T: _) => ()
        | constraint (_T: _, 0) => ()
        | constraint ((h: any ~ t: any), i: nat) => cons(h, go(t, i - 1))
        | panic
};
let constraint drop: any = constraint lst: List(Any) => constraint n: nat => {
    loop go: constraint (t: any, i: nat) = (lst, n);
    match (t, i)
        | constraint ((), _T: _) => ()
        | constraint (l: any, 0) => l
        | constraint ((_T: _ ~ t: any), i: nat) => go(t, i - 1)
        | panic
};
let constraint find: any = constraint lst: List(Any) => constraint pred: any => {
    let constraint go: any = dyn_rec go: match
        | assert () => {
            let constraint Nothing::(v: any) = maybe_pkg;
            v
        }
        | constraint (h: any ~ t: any) => if pred(h)
            then {
                let constraint Just::(v: any) = maybe_pkg;
                v(h)
            }
            else go(t)
        | panic;
    go(lst)
};
let constraint allof: any = constraint lst: List(Any) => constraint pred: any => {
    let constraint go: any = dyn_rec go: match
        | assert () => true
        | constraint (h: any ~ t: any) => if pred(h)
            then go(t)
            else false
        | panic;
    go(lst)
};
let constraint anyof: any = constraint lst: List(Any) => constraint pred: any => {
    let constraint go: any = dyn_rec go: match
        | assert () => false
        | constraint (h: any ~ t: any) => if pred(h)
            then true
            else go(t)
        | panic;
    go(lst)
};

List::List &
Nat::List &
Greater::Greater &
Range::Range &
Exact::Exact &
Modular::Modular &
Nil::Nil &
cons::cons &
head::head &
tail::tail &
is_nil::is_nil &
iter::iter &
map::map &
len::len &
filter::filter &
fold::fold &
foldr::foldr &
append::append &
reverse::reverse &
nth::nth &
take::take &
drop::drop &
find::find &
allof::allof &
anyof::anyof &
iteri::iteri