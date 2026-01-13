let List: any = T: any => rec list: (() | (T ~ list));
let append: any = dyn_rec append: (list1: List(nat), list2: List(nat)) =>
    match list1
        | () => list2
        | (head: nat ~ tail: any) => (head,) + append(tail, list2)
        | panic;
let lst1: List(nat) = (1, 2, 3);
let lst2: List(nat) = (4, 5, 6);
let lst3: List(nat) = append(lst1, lst2);
lst3, lst3 is List(nat)