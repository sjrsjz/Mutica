let int_list: any = rec list: (() | (int, list));
let append: any = rec appnd: (list1: any, list2: any) |->
    match list1
        | () => list2
        | (head: int, tail: any) => (head, append(tail, list2))
        | panic;
let lst1: any = @(1, 2, 3);
let lst2: any = @(4, 5, 6);
let lst3: any = append(lst1, lst2);
lst3, lst3 <: int_list