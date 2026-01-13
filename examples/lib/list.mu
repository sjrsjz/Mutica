let maybe_pkg: any = import "maybe.mu";
let Any::(Any: any) = import "any.mu";
let while_condition::(while: lambda) = import "controlflow.mu";
let deref::($"op#not": lambda) = import "mutable.mu";

let List: lambda = T: any => (!..T);

let Greater: lambda = (T: any, n: nat) => {
    let prefix: any = mut ();
    let i: any = mut 0;
    discard while delay (!i < n) delay {
        discard prefix := !prefix + (T,);
        i := !i + 1
    };
    !prefix + (!..T)
};

let Range: lambda = (T: any, min: nat, max: nat) => {
    let tuple: any = mut ();
    let final: any = mut never;
    let i: any = mut 0;
    discard while delay (!i < max) delay {
        discard if !i >= min then {
            final := (!final | !tuple) 
        } else ();
        discard tuple := !tuple + (T,);
        i := !i + 1
    };
    !final
};

let Exact: lambda = (T: any, n: nat) => {
    let tuple: any = mut ();
    let i: any = mut 0;
    discard while delay (!i < n) delay {
        discard tuple := !tuple + (T,);
        i := !i + 1
    };
    !tuple
};

let Modular: lambda = (T: any, range_len: nat, prefix_len: nat) => {
    match range_len
        | 0 => Exact(T, prefix_len)
        | _T: any => {
            let seq: any = dyn_rec tail: {
                let seq: any = mut (T ~ tail);
                let j: any = mut 1;
                discard while delay (!j < range_len) delay {
                    discard seq := (T,) + !seq;
                    j := !j + 1
                };
                () | !seq
            };
            match prefix_len
                | 0 => seq
                | _T: any => {
                    let final: any = mut (T ~ seq);
                    let i: any = mut 1;
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

let Nil: any = ();
let cons: lambda = (head: any, tail: any) => (head,) + tail;
let head: lambda = match
    | (h: any ~ _T: _) => h
    | panic;
let tail: lambda = match
    | (_T: _ ~ t: any) => t
    | panic;
let is_nil: lambda = match
    | () => true
    | _T: _ => false
    | panic;
let iter: lambda = lst: List(Any) => f: lambda => {
    loop go: t: any = lst;
    match t
        | () => ()
        | (h: any ~ t: any) => {
            discard f(h);
            go(t)
        }
        | panic
};
let iteri: lambda = lst: List(Any) => f: lambda => {
    loop go: (t: any, index: nat) = (lst, 0);
    match t
        | () => ()
        | (h: any ~ t: any) => {
            discard f(index, h);
            go(t, index + 1)
        }
        | panic
};
let map: lambda = lst: List(Any) => f: lambda => {
    loop go: t: any = lst;
    match t
        | () => ()
        | (h: any ~ t: any) => cons(f(h), go(t))
        | panic
};
let len: lambda = lst: List(Any) => {
    loop go: t: any = lst;
    match t
        | () => 0
        | (_T: _ ~ t: any) => 1 + go(t)
        | panic
};
let filter: lambda = lst: List(Any) => pred: lambda => {
    loop go: t: any = lst;
    match t
        | () => ()
        | (h: any ~ t: any) => if pred(h)
            then cons(h, go(t))
            else go(t)
        | panic
};
let fold: lambda = 
    lst: List(Any) => 
    acc: any => 
    f: lambda => {
    loop go: (t: any, a: any) = (lst, acc);
    match t
        | () => a
        | (h: any ~ t: any) => go(t, f(a, h))
        | panic
};
let foldr: lambda = 
    lst: List(Any) => 
    acc: any => 
    f: lambda => {
    loop go: t: any = lst;
    match t
        | () => acc
        | (h: any ~ t: any) => f(h, go(t))
        | panic
};
let append: lambda = 
    lst1: List(Any) => 
    lst2: List(Any) => {
    lst1 + lst2
};
let reverse: lambda = lst: List(Any) => {
    loop go: t: any = (lst, ());
    match t
        | ((), acc: any) => acc
        | ((h: any ~ t: any), acc: any) => go(t, cons(h, acc))
        | panic
};
let nth: lambda = lst: List(Any) => n: nat => {
    loop go: (t: any, i: nat) = (lst, n);
    match (t, i)
        | ((h: any ~ _T: _) , 0) => h
        | ((_T: _ ~ t: any), i: nat) => go(t, i - 1)
        | panic
};
let take: lambda = lst: List(Any) => n: nat => {
    loop go: (t: any, i: nat) = (lst, n);
    match (t, i)
        | ((), _T: _) => ()
        | (_T: _, 0) => ()
        | ((h: any ~ t: any), i: nat) => cons(h, go(t, i - 1))
        | panic
};
let drop: lambda = lst: List(Any) => n: nat => {
    loop go: (t: any, i: nat) = (lst, n);
    match (t, i)
        | ((), _T: _) => ()
        | (l: any, 0) => l
        | ((_T: _ ~ t: any), i: nat) => go(t, i - 1)
        | panic
};
let find: lambda = lst: List(Any) => pred: lambda => {
    let go: lambda = dyn_rec go: match
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
let allof: lambda = lst: List(Any) => pred: lambda => {
    let go: lambda = dyn_rec go: match
        | () => true
        | (h: any ~ t: any) => if pred(h)
            then go(t)
            else false
        | panic;
    go(lst)
};
let anyof: lambda = lst: List(Any) => pred: lambda => {
    let go: lambda = dyn_rec go: match
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