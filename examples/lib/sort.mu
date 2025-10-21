let list_pkg: any = import "list.mu";
let List: any = list_pkg.List;

// 归并两个已排序的列表
let merge: any = (cmp: any, lst1: List(any), lst2: List(any)) -> {
    loop go: t: any = (lst1, lst2);
    match t
        | ((), l2: any) => l2
        | (l1: any, ()) => l1
        | ((h1: any, t1: any), (h2: any, t2: any)) => if cmp(h1, h2)
            then (h1, go(t1, (h2, t2)))
            else (h2, go((h1, t1), t2))
        | panic
};

// 将列表分为两半
let split: any = lst: List(any) -> {
    let len: any = list_pkg.len lst;
    let mid: int = len / 2;
    let first_half: any = list_pkg.take lst mid;
    let second_half: any = list_pkg.drop lst mid;
    (first_half, second_half)
};

// 归并排序主函数
let merge_sort: any = cmp: any -> lst: List(any) -> {
    loop go: t: any = lst;
    match t
        | () => ()
        | v: (any, ()) => v
        | l: any => {
            let (left: any, right: any) = split(l);
            let sorted_left: any = go(left);
            let sorted_right: any = go(right);
            merge(cmp, sorted_left, sorted_right)
        }
        | panic
};

merge_sort::merge_sort