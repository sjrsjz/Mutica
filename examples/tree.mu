// 二叉树示例
let Leaf: any = value: any |-> Leaf::value;
let Node: any = (left: any, right: any, value: any) |-> Node::(left, right, value);

// 树的大小
let tree_size: any = 
    rec size: tree: (Empty::() | Leaf::any | Node::(any, any, any)) |->
        match tree
            | Empty::() => 0
            | Leaf::(any) => 1
            | Node::(left: any, right: any, any) => 
                1 + size(left) + size(right)
            | panic;

// 树的高度
let tree_height: any = 
    rec height: tree: (Empty::() | Leaf::any | Node::(any, any, any)) |->
        match tree
            | Empty::() => 0
            | Leaf::(any) => 1
            | Node::(left: any, right: any, any) => {
                let lh: int = height(left);
                let rh: int = height(right);
                1 + (match lh > rh | false => rh | true => lh | panic)
            }
            | panic;

// 树的求和
let tree_sum: any = 
    rec ts: tree: (Empty::() | Leaf::int | Node::(any, any, int)) |->
        match tree
            | Empty::() => 0
            | Leaf::(val: int) => val
            | Node::(left: any, right: any, val: int) => 
                val + ts(left) + ts(right)
            | panic;

// 创建示例树
let mytree: any = 
    Node(
        Node(Leaf 3, Leaf 5, 2),
        Leaf 7,
        1
    );

tree_size mytree, tree_height mytree, tree_sum mytree
