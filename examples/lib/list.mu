let constraint maybe_pkg: any = import "maybe.mu";
let constraint Any::(Any: any) = import "any.mu";
let constraint while_condition::(while: lambda) = import "controlflow.mu";
let constraint deref::($"op#not": lambda) = import "mutable.mu";

let constraint List: lambda = constraint T: any => (!..T);

let constraint Greater: lambda = constraint (T: any, n: nat) => {
    let constraint prefix: any = mut ();
    let constraint i: any = mut 0;
    discard while delay (!i < n) delay {
        discard prefix := !prefix + (T,);
        i := !i + 1
    };
    !prefix + (!..T)
};

let constraint Range: lambda = constraint (T: any, min: nat, max: nat) => {
    let constraint tuple: any = mut ();
    let constraint final: any = mut never;
    let constraint i: any = mut 0;
    discard while delay (!i < max) delay {
        discard if !i >= min then {
            final := (!final | !tuple) 
        } else ();
        discard tuple := !tuple + (T,);
        i := !i + 1
    };
    !final
};

let constraint Exact: lambda = constraint (T: any, n: nat) => {
    let constraint tuple: any = mut ();
    let constraint i: any = mut 0;
    discard while delay (!i < n) delay {
        discard tuple := !tuple + (T,);
        i := !i + 1
    };
    !tuple
};

let constraint Modular: lambda = constraint (T: any, range_len: nat, prefix_len: nat) => {
    match range_len
        | assert 0 => Exact(T, prefix_len)
        | constraint _T: any => {
            let constraint seq: any = dyn_rec tail: {
                let constraint seq: any = mut (T ~ tail);
                let constraint j: any = mut 1;
                discard while delay (!j < range_len) delay {
                    discard seq := (T,) + !seq;
                    j := !j + 1
                };
                () | !seq
            };
            match prefix_len
                | assert 0 => seq
                | constraint _T: any => {
                    let constraint final: any = mut (T ~ seq);
                    let constraint i: any = mut 1;
                    discard while delay (!i < prefix_len) delay {
                        discard final := (T,) + !final;
                        i := !i + 1
                    };
                    !final
                }
                | panic
        }
        | panic
};

let constraint Nil: any = ();
let constraint cons: lambda = constraint (head: any, tail: any) => (head,) + tail;
let constraint head: lambda = match
    | constraint (h: any ~ _T: _) => h
    | panic;
let constraint tail: lambda = match
    | constraint (_T: _ ~ t: any) => t
    | panic;
let constraint is_nil: lambda = match
    | assert () => true
    | constraint _T: _ => false
    | panic;
let constraint iter: lambda = constraint lst: List(Any) => constraint f: lambda => {
    loop go: constraint t: any = lst;
    match t
        | assert () => ()
        | constraint (h: any ~ t: any) => {
            discard f(h);
            go(t)
        }
        | panic
};
let constraint iteri: lambda = constraint lst: List(Any) => constraint f: lambda => {
    loop go: constraint (t: any, index: nat) = (lst, 0);
    match t
        | assert () => ()
        | constraint (h: any ~ t: any) => {
            discard f(index, h);
            go(t, index + 1)
        }
        | panic
};
let constraint map: lambda = constraint lst: List(Any) => constraint f: lambda => {
    loop go: constraint t: any = lst;
    match t
        | assert () => ()
        | constraint (h: any ~ t: any) => cons(f(h), go(t))
        | panic
};
let constraint len: lambda = constraint lst: List(Any) => {
    loop go: constraint t: any = lst;
    match t
        | assert () => 0
        | constraint (_T: _ ~ t: any) => 1 + go(t)
        | panic
};
let constraint filter: lambda = constraint lst: List(Any) => constraint pred: lambda => {
    loop go: constraint t: any = lst;
    match t
        | assert () => ()
        | constraint (h: any ~ t: any) => if pred(h)
            then cons(h, go(t))
            else go(t)
        | panic
};
let constraint fold: lambda = 
    constraint lst: List(Any) => 
    constraint acc: any => 
    constraint f: lambda => {
    loop go: constraint (t: any, a: any) = (lst, acc);
    match t
        | assert () => a
        | constraint (h: any ~ t: any) => go(t, f(a, h))
        | panic
};
let constraint foldr: lambda = 
    constraint lst: List(Any) => 
    constraint acc: any => 
    constraint f: lambda => {
    loop go: constraint t: any = lst;
    match t
        | assert () => acc
        | constraint (h: any ~ t: any) => f(h, go(t))
        | panic
};
let constraint append: lambda = 
    constraint lst1: List(Any) => 
    constraint lst2: List(Any) => {
    lst1 + lst2
};
let constraint reverse: lambda = constraint lst: List(Any) => {
    loop go: constraint t: any = (lst, ());
    match t
        | constraint ((), acc: any) => acc
        | constraint ((h: any ~ t: any), acc: any) => go(t, cons(h, acc))
        | panic
};
let constraint nth: lambda = constraint lst: List(Any) => constraint n: nat => {
    loop go: constraint (t: any, i: nat) = (lst, n);
    match (t, i)
        | constraint ((h: any ~ _T: _) , 0) => h
        | constraint ((_T: _ ~ t: any), i: nat) => go(t, i - 1)
        | panic
};
let constraint take: lambda = constraint lst: List(Any) => constraint n: nat => {
    loop go: constraint (t: any, i: nat) = (lst, n);
    match (t, i)
        | constraint ((), _T: _) => ()
        | constraint (_T: _, 0) => ()
        | constraint ((h: any ~ t: any), i: nat) => cons(h, go(t, i - 1))
        | panic
};
let constraint drop: lambda = constraint lst: List(Any) => constraint n: nat => {
    loop go: constraint (t: any, i: nat) = (lst, n);
    match (t, i)
        | constraint ((), _T: _) => ()
        | constraint (l: any, 0) => l
        | constraint ((_T: _ ~ t: any), i: nat) => go(t, i - 1)
        | panic
};
let constraint find: lambda = constraint lst: List(Any) => constraint pred: lambda => {
    let constraint go: lambda = dyn_rec go: match
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
let constraint allof: lambda = constraint lst: List(Any) => constraint pred: lambda => {
    let constraint go: lambda = dyn_rec go: match
        | assert () => true
        | constraint (h: any ~ t: any) => if pred(h)
            then go(t)
            else false
        | panic;
    go(lst)
};
let constraint anyof: lambda = constraint lst: List(Any) => constraint pred: lambda => {
    let constraint go: lambda = dyn_rec go: match
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