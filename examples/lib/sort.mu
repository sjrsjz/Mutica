let constraint list_pkg: any = import "list.mu";
let constraint Any::(Any: any) = import "any.mu";
let constraint {
    List::(List: any) &
    cons::(cons: any) &
    len::(len: any) &
    drop::(drop: any) &
    take::(take: any)
} = list_pkg;
// 归并两个已排序的列表
let constraint merge: any = constraint (cmp: any, lst1: List(Any), lst2: List(Any)) => {
    loop merge_go: constraint t: any = (lst1, lst2);
    match t
        | constraint ((), l2: any) => l2
        | constraint (l1: any, ()) => l1
        | constraint ((h1: any ~ t1: any), (h2: any ~ t2: any)) => 
            if cmp(h1, h2)
                then cons(h1, merge_go(t1, cons(h2, t2)))
                else cons(h2, merge_go(cons(h1, t1), t2))
        | panic
};

// 将列表分为两半
let constraint split: any = constraint lst: List(Any) => {
    let constraint len: any = len lst;
    let constraint mid: nat = len / 2;
    let constraint first_half: any = take lst mid;
    let constraint second_half: any = drop lst mid;
    (first_half, second_half)
};

// 归并排序主函数
let constraint merge_sort: any = constraint cmp: any => constraint lst: List(Any) =>  {
    loop go: constraint t: any = lst;
    match t
        | assert () => ()
        | constraint (v: any ~ ()) => cons(v, ())
        | constraint l: any => {
            let constraint (left: any, right: any) = split(l);
            let constraint sorted_left: any = go(left);
            let constraint sorted_right: any = go(right);
            merge(cmp, sorted_left, sorted_right)
        }
        | panic
};


// 快速排序
let constraint quick_sort: any = constraint cmp: any => constraint lst: List(Any) => {
    loop go: constraint t: any = lst;
    match t
        | assert () => ()
        | constraint (v: any ~ ()) => v
        | constraint (pivot: any ~ rest: any) => {
            // 分区函数
            let constraint partition: any = constraint l: List(Any) => {
                loop part: constraint pt: any = (l, (), ());
                let constraint (lst_p: any, smaller: any, larger: any) = pt;
                match lst_p
                    | assert () => (smaller, larger)
                    | constraint (h: any ~ t: any) => if cmp(h, pivot)
                        then part(t, cons(h, smaller), larger)
                        else part(t, smaller, cons(h, larger))
                    | panic
            };
            let constraint (small: any, large: any) = partition(rest);
            let constraint sorted_small: any = go(small);
            let constraint sorted_large: any = go(large);
            // 连接三部分
            loop concat: constraint t2: any = sorted_small;
            match t2
                | assert () => cons(pivot, sorted_large)
                | constraint (h: any ~ t: any) => cons(h, concat(t))
                | panic
        }
        | panic
};

// 插入排序
let constraint insert_sort: any = constraint cmp: any => constraint lst: List(Any) => {
    // 将元素插入已排序列表
    let constraint insert: any = constraint (x: any, sorted: List(Any)) => {
        loop go: constraint t: any = sorted;
        match t
            | assert () => cons(x, ())
            | constraint (h: any ~ rest: any) => if cmp(x, h)
                then cons(x, t)
                else cons(h, go(rest))
            | panic
    };
    
    loop go: constraint t: any = lst;
    match t
        | assert () => ()
        | constraint (h: any ~ t: any) => insert(h, go(t))
        | panic
};

merge_sort::merge_sort &
quick_sort::quick_sort &
insert_sort::insert_sort