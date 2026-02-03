let maybe_pkg: any = import "maybe.mu";
let while_condition::(while: any) = import "controlflow.mu";
let deref::($"op#not": any) = import "mutable.mu";

let List: any = T: any => (!..T);

let Greater: any = (T: any, n: nat) => {
    let prefix: any = mut ();
    let i: any = mut 0;
    while delay (!i < n) delay {
        prefix := !prefix + (T,);
        i := !i + 1
    };
    !prefix + (!..T)
};

let Range: any = (T: any, min: nat, max: nat) => {
    let tuple: any = mut ();
    let final: any = mut never;
    let i: any = mut 0;
    while delay (!i < max) delay {
        if !i >= min then {
            final := (!final | !tuple) 
        } else ();
        tuple := !tuple + (T,);
        i := !i + 1
    };
    !final
};

let Exact: any = (T: any, n: nat) => {
    let tuple: any = mut ();
    let i: any = mut 0;
    while delay (!i < n) delay {
        tuple := !tuple + (T,);
        i := !i + 1
    };
    !tuple
};

let Modular: any = (T: any, range_len: nat, prefix_len: nat) => {
    match range_len
        | 0 => Exact(T, prefix_len)
        | _T: any => {
            let seq: any = dyn_rec tail: {
                let seq: any = mut (T ~ tail);
                let j: any = mut 1;
                while delay (!j < range_len) delay {
                    seq := (T,) + !seq;
                    j := !j + 1
                };
                () | !seq
            };
            match prefix_len
                | 0 => seq
                | _T: any => {
                    let final: any = mut (T ~ seq);
                    let i: any = mut 1;
                    while delay (!i < prefix_len) delay {
                        final := (T,) + !final;
                        i := !i + 1
                    };
                    !final
                }
                | panic
        }
        | panic
};

let Nil: any = ();
let cons: any = (head: any, tail: any) => (head,) + tail;
let head: any = match
    | (h: any ~ _T: _) => h
    | panic;
let tail: any = match
    | (_T: _ ~ t: any) => t
    | panic;
let is_nil: any = match
    | () => true
    | _T: _ => false
    | panic;
let iter: any = lst: List(any) => f: (lambda | v: never | panic) => {
    loop go: t: any = lst;
    match t
        | () => ()
        | (h: any ~ t: any) => {
            f(h);
            go(t)
        }
        | panic
};
let iteri: any = lst: List(any) => f: (lambda | (index: nat, v: never) | panic) => {
    loop go: (t: any, index: nat) = (lst, 0);
    match t
        | () => ()
        | (h: any ~ t: any) => {
            f(index, h);
            go(t, index + 1)
        }
        | panic
};
let map: any = lst: List(any) => f: (lambda | v: never | panic) => {
    loop go: t: any = lst;
    match t
        | () => ()
        | (h: any ~ t: any) => cons(f(h), go(t))
        | panic
};
let len: any = lst: List(any) => {
    loop go: t: any = lst;
    match t
        | () => 0
        | (_T: _ ~ t: any) => 1 + go(t)
        | panic
};
let filter: any = lst: List(any) => pred: (lambda | v: never | panic) => {
    loop go: t: any = lst;
    match t
        | () => ()
        | (h: any ~ t: any) => if pred(h)
            then cons(h, go(t))
            else go(t)
        | panic
};
let fold: any = 
    lst: List(any) => 
    acc: any => 
    f: (lambda | (acc: never, v: never) | panic) => {
    loop go: (t: any, a: any) = (lst, acc);
    match t
        | () => a
        | (h: any ~ t: any) => go(t, f(a, h))
        | panic
};
let foldr: any = 
    lst: List(any) => 
    acc: any => 
    f: (lambda | (v: never, acc: never) | panic) => {
    loop go: t: any = lst;
    match t
        | () => acc
        | (h: any ~ t: any) => f(h, go(t))
        | panic
};
let append: any = 
    lst1: List(any) => 
    lst2: List(any) => {
    lst1 + lst2
};
let reverse: any = lst: List(any) => {
    loop go: t: any = (lst, ());
    match t
        | ((), acc: any) => acc
        | ((h: any ~ t: any), acc: any) => go(t, cons(h, acc))
        | panic
};
let nth: any = lst: List(any) => n: nat => {
    loop go: (t: any, i: nat) = (lst, n);
    match (t, i)
        | ((h: any ~ _T: _) , 0) => h
        | ((_T: _ ~ t: any), i: nat) => go(t, i - 1)
        | panic
};
let take: any = lst: List(any) => n: nat => {
    loop go: (t: any, i: nat) = (lst, n);
    match (t, i)
        | ((), _T: _) => ()
        | (_T: _, 0) => ()
        | ((h: any ~ t: any), i: nat) => cons(h, go(t, i - 1))
        | panic
};
let drop: any = lst: List(any) => n: nat => {
    loop go: (t: any, i: nat) = (lst, n);
    match (t, i)
        | ((), _T: _) => ()
        | (l: any, 0) => l
        | ((_T: _ ~ t: any), i: nat) => go(t, i - 1)
        | panic
};
let find: any = lst: List(any) => pred: (lambda | v: never | panic) => {
    let go: any = dyn_rec go: match
        | () => {
            let Nothing::(v: any) = maybe_pkg;
            v
        }
        | (h: any ~ t: any) => if pred(h)
            then {
                let Just::(v: any) = maybe_pkg;
                v(h)
            }
            else go(t)
        | panic;
    go(lst)
};
let allof: any = lst: List(any) => pred: (lambda | v: never | panic) => {
    let go: any = dyn_rec go: match
        | () => true
        | (h: any ~ t: any) => if pred(h)
            then go(t)
            else false
        | panic;
    go(lst)
};
let anyof: any = lst: List(any) => pred: (lambda | v: never | panic) => {
    let go: any = dyn_rec go: match
        | () => false
        | (h: any ~ t: any) => if pred(h)
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