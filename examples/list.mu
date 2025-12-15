let constraint List: any = constraint T: any => rec list: (() | (T ~ list));
let constraint append: any = dyn_rec append: constraint (list1: List(nat), list2: List(nat)) =>
    match list1
        | assert () => list2
        | constraint (head: nat ~ tail: any) => (head,) + append(tail, list2)
        | panic;
let constraint lst1: List(nat) = (1, 2, 3);
let constraint lst2: List(nat) = (4, 5, 6);
let constraint lst3: List(nat) = append(lst1, lst2);
lst3, lst3 is List(nat)