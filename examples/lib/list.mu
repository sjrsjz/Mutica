let maybe_pkg: any = import "maybe.mu";
let List: any = T: any |-> rec list: (() | (T, list));
let Nil: any = ();
let cons: any = (head: any, tail: any) |-> (head, tail);
let head: any = match
    | (h: any, _) => h
    | panic;
let tail: any = match
    | (_, t: any) => t
    | panic;
let is_nil: any = match
    | () => true
    | _ => false
    | panic;
let iter: any = lst: List(any) |-> f: any |-> {
    let loop: any = rec go: match
        | () => ()
        | (h: any, t: any) => {
            discard f(h);
            go(t)
        }
        | panic;
    loop(lst)
};
let map: any = lst: List(any) |-> f: any |-> {
    let loop: any = rec go: match
        | () => ()
        | (h: any, t: any) => cons(f(h), go(t))
        | panic;
    loop(lst)
};
let len: any = lst: List(any) |-> {
    let loop: any = rec go: match
        | () => 0
        | (_, t: any) => 1 + go(t)
        | panic;
    loop(lst)
};
let filter: any = lst: List(any) |-> pred: any |-> {
    let loop: any = rec go: match
        | () => ()
        | (h: any, t: any) => match pred(h)
            | false => go(t)
            | true => cons(h, go(t))
            | panic
        | panic;
    loop(lst)
};
let fold: any = lst: List(any) |-> acc: any |-> f: any |-> {
    let loop: any = rec go: match
        | ((), a: any) => a
        | ((h: any, t: any), a: any) => go(t, f(a, h))
        | panic;
    loop(lst, acc)
};
let foldr: any = lst: List(any) |-> acc: any |-> f: any |-> {
    let loop: any = rec go: match
        | () => acc
        | (h: any, t: any) => f(h, go(t))
        | panic;
    loop(lst)
};
let append: any = lst1: List(any) |-> lst2: List(any) |-> {
    let loop: any = rec go: match
        | () => lst2
        | (h: any, t: any) => cons(h, go(t))
        | panic;
    loop(lst1)
};
let reverse: any = lst: List(any) |-> {
    let loop: any = rec go: match
        | ((), acc: any) => acc
        | ((h: any, t: any), acc: any) => go(t, cons(h, acc))
        | panic;
    loop(lst, ())
};
let nth: any = lst: List(any) |-> n: int |-> {
    let loop: any = rec go: match
        | ((h: any, _), 0) => h
        | ((_, t: any), i: any) => go(t, i - 1)
        | panic;
    loop(lst, n)
};
let take: any = lst: List(any) |-> n: int |-> {
    let loop: any = rec go: match
        | ((), _) => ()
        | (_, 0) => ()
        | ((h: any, t: any), i: any) => cons(h, go(t, i - 1))
        | panic;
    loop(lst, n)
};
let drop: any = lst: List(any) |-> n: int |-> {
    let loop: any = rec go: match
        | ((), _) => ()
        | (l: any, 0) => l
        | ((_, t: any), i: any) => go(t, i - 1)
        | panic;
    loop(lst, n)
};
let find: any = lst: List(any) |-> pred: any |-> {
    let loop: any = rec go: match
        | () => maybe_pkg.Nothing
        | (h: any, t: any) => match pred(h)
            | false => go(t)
            | true => maybe_pkg.Just(h)
            | panic
        | panic;
    loop(lst)
};
let list_all: any = lst: List(any) |-> pred: any |-> {
    let loop: any = rec go: match
        | () => true
        | (h: any, t: any) => match pred(h)
            | false => false
            | true => go(t)
            | panic
        | panic;
    loop(lst)
};
let list_any: any = lst: List(any) |-> pred: any |-> {
    let loop: any = rec go: match
        | () => false
        | (h: any, t: any) => match pred(h)
            | false => go(t)
            | true => true
            | panic
        | panic;
    loop(lst)
};

List::List &
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
list_all::list_all &
list_any::list_any