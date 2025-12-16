// 二叉树示例
let constraint Leaf: any = constraint value: any => Leaf::value;
let constraint Node: any = constraint (left: any, right: any, value: any) => Node::(left, right, value);
let constraint Empty: any = Empty::();
let constraint Tree: any = constraint T: any => rec tree: (Empty::() | Leaf::T | Node::(tree, tree, T));

// 树的大小
let constraint tree_size: any = 
    dyn_rec size: match
        | assert Empty => 0
        | assert Leaf(any) => 1
        | constraint Node::(left: any, right: any, any) => 
            1 + size(left) + size(right)
        | panic;

// 树的高度
let constraint tree_height: any = 
    dyn_rec height: match
        | assert Empty => 0
        | assert Leaf(any) => 1
        | constraint Node::(left: any, right: any, any) => {
            let constraint lh: nat = height(left);
            let constraint rh: nat = height(right);
            1 + (if lh > rh then lh else rh)
        }
        | panic;

// 树的求和
let constraint tree_sum: any = 
    dyn_rec ts: match
        | assert Empty => 0
        | constraint Leaf::(val: nat) => val
        | constraint Node::(left: any, right: any, val: nat) => 
            val + ts(left) + ts(right)
        | panic;

// 创建示例树
let constraint mytree: any = 
    Node(
        Node(Leaf 3, Leaf 5, 2),
        Leaf 7,
        1
    );

tree_size mytree, tree_height mytree, tree_sum mytree, mytree is Tree(nat), Tree(nat) is Tree(any)
