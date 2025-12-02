let list_pkg: any = import "list.mu";
let List::(List: any) = list_pkg;
let cons::(cons: any) = list_pkg;
let len::(len: any) = list_pkg;
let drop::(drop: any) = list_pkg;
let take::(take: any) = list_pkg;
// 归并两个已排序的列表
let merge: any = (cmp: any, lst1: List(any), lst2: List(any)) => {
    loop merge_go: t: any = (lst1, lst2);
    match t
        | ((), l2: any) => l2
        | (l1: any, ()) => l1
        | ((h1: any) @ (t1: any), (h2: any) @ (t2: any)) => 
            if cmp(h1, h2)
                then cons(h1, merge_go(t1, cons(h2, t2)))
                else cons(h2, merge_go(cons(h1, t1), t2))
        | panic
};

// 将列表分为两半
let split: any = lst: List(any) => {
    let len: any = len lst;
    let mid: nat = len / 2;
    let first_half: any = take lst mid;
    let second_half: any = drop lst mid;
    (first_half, second_half)
};

// 归并排序主函数
let merge_sort: any = cmp: any => lst: List(any) =>  {
    loop go: t: any = lst;
    match t
        | () => ()
        | (v: any) @ () => cons(v, ())
        | l: any => {
            let (left: any, right: any) = split(l);
            let sorted_left: any = go(left);
            let sorted_right: any = go(right);
            merge(cmp, sorted_left, sorted_right)
        }
        | panic
};


// 快速排序
let quick_sort: any = cmp: any => lst: List(any) => {
    loop go: t: any = lst;
    match t
        | () => ()
        | v: (any @ ()) => v
        | (pivot: any) @ (rest: any) => {
            // 分区函数
            let partition: any = l: List(any) => {
                loop part: pt: any = (l, (), ());
                let (lst_p: any, smaller: any, larger: any) = pt;
                match lst_p
                    | () => (smaller, larger)
                    | (h: any) @ (t: any) => if cmp(h, pivot)
                        then part(t, cons(h, smaller), larger)
                        else part(t, smaller, cons(h, larger))
                    | panic
            };
            let (small: any, large: any) = partition(rest);
            let sorted_small: any = go(small);
            let sorted_large: any = go(large);
            // 连接三部分
            loop concat: t2: any = sorted_small;
            match t2
                | () => cons(pivot, sorted_large)
                | (h: any) @ (t: any) => cons(h, concat(t))
                | panic
        }
        | panic
};

// 插入排序
let insert_sort: any = cmp: any => lst: List(any) => {
    // 将元素插入已排序列表
    let insert: any = (x: any, sorted: List(any)) => {
        loop go: t: any = sorted;
        match t
            | () => cons(x, ())
            | (h: any) @ (rest: any) => if cmp(x, h)
                then cons(x, t)
                else cons(h, go(rest))
            | panic
    };
    
    loop go: t: any = lst;
    match t
        | () => ()
        | (h: any) @ (t: any) => insert(h, go(t))
        | panic
};

merge_sort::merge_sort &
quick_sort::quick_sort &
insert_sort::insert_sort