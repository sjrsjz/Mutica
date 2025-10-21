let List: any = (T: any) -> rec list: (() | (T, list));
let append: any = rec append: (list1: List(int), list2: List(int)) ->
    match list1
        | () => list2
        | (head: int, tail: any) => (head, append(tail, list2))
        | panic;
let lst1: List(int) = @(1, 2, 3);
let lst2: List(int) = @(4, 5, 6);
let lst3: List(int) = append(lst1, lst2);
lst3, lst3 <: List(int)