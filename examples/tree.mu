let Any::(Any: any) = import "lib/any.mu";
// 二叉树示例
let Leaf: any = value: any => Leaf::value;
let Node: any = (left: any, right: any, value: any) => Node::(left, right, value);
let Empty: any = Empty::();
let Tree: any = T: any => rec tree: (Empty::() | Leaf::T | Node::(tree, tree, T));

// 树的大小
let tree_size: any = 
    dyn_rec size: match
        | Empty => 0
        | Leaf(Any) => 1
        | Node::(left: any, right: any, _T: any) => 
            1 + size(left) + size(right)
        | panic;

// 树的高度
let tree_height: any = 
    dyn_rec height: match
        | Empty => 0
        | Leaf(Any) => 1
        | Node::(left: any, right: any, _T: any) => {
            let lh: nat = height(left);
            let rh: nat = height(right);
            1 + (if lh > rh then lh else rh)
        }
        | panic;

// 树的求和
let tree_sum: any = 
    dyn_rec ts: match
        | Empty => 0
        | Leaf::(val: nat) => val
        | Node::(left: any, right: any, val: nat) => 
            val + ts(left) + ts(right)
        | panic;

// 创建示例树
let mytree: any = 
    Node(
        Node(Leaf 3, Leaf 5, 2),
        Leaf 7,
        1
    );

tree_size mytree, tree_height mytree, tree_sum mytree, mytree is Tree(nat), Tree(nat) is Tree(Any)
