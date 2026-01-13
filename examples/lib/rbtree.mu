let maybe_pkg: any = import "maybe.mu";
let {
    int::(int: any) &
    Lt::($"op#lt": lambda) &
    Gt::($"op#gt": lambda)
} = import "int.mu";
let Just::(Just: lambda) = maybe_pkg;
let Nothing::(Nothing: any) = maybe_pkg;
let Any::(Any: any) = import "any.mu";

// 颜色定义
let Red: any = Red::();
let Black: any = Black::();
let Color: any = (Red | Black);

// 红黑树定义
// Tree: Empty | Node(color, key, value, left, right)
let Tree: lambda = (K: any, V: any) => rec tree: (
    Empty::() | 
    Node::(Color, K, V, tree, tree)
);

// 创建空树
let empty: any = Empty::();

// 平衡函数 - 处理红黑树的4种违规情况
let balance: lambda = t: Tree(Any, Any) => 
    match t
        // 情况1: 左-左红红
        | Node::(Black, z: any, zv: any, Node::(Red, y: any, yv: any, Node::(Red, x: any, xv: any, a: any, b: any), c: any), d: any) =>
            Node::(Red, y, yv, Node::(Black, x, xv, a, b), Node::(Black, z, zv, c, d))
        // 情况2: 左-右红红
        | Node::(Black, z: any, zv: any, Node::(Red, x: any, xv: any, a: any, Node::(Red, y: any, yv: any, b: any, c: any)), d: any) =>
            Node::(Red, y, yv, Node::(Black, x, xv, a, b), Node::(Black, z, zv, c, d))
        // 情况3: 右-左红红
        | Node::(Black, x: any, xv: any, a: any, Node::(Red, z: any, zv: any, Node::(Red, y: any, yv: any, b: any, c: any), d: any)) =>
            Node::(Red, y, yv, Node::(Black, x, xv, a, b), Node::(Black, z, zv, c, d))
        // 情况4: 右-右红红
        | Node::(Black, x: any, xv: any, a: any, Node::(Red, y: any, yv: any, b: any, Node::(Red, z: any, zv: any, c: any, d: any))) =>
            Node::(Red, y, yv, Node::(Black, x, xv, a, b), Node::(Black, z, zv, c, d))
        // 其他情况保持不变
        | tree: any => tree
        | panic;

// 插入辅助函数
let insert_helper: lambda = cmp: lambda => tree: Tree(Any, Any) => key: any => value: any => {
    loop go: t: any = tree;
    match t
        | Empty::() => Node::(Red, key, value, Empty::(), Empty::())
        | Node::(color: any, k: any, v: any, left: any, right: any) => {
            let cmp_result: int = cmp(key, k);
            if cmp_result < 0
                then balance(Node::(color, k, v, go(left), right))
                else if cmp_result > 0
                    then balance(Node::(color, k, v, left, go(right)))
                    else Node::(color, key, value, left, right)  // 更新值
        }
        | panic
};

// 插入函数 - 确保根节点是黑色
let insert: lambda = cmp: lambda => tree: Tree(Any, Any) => key: any => value: any => {
    let result: any = insert_helper(cmp)(tree)(key)(value);
    match result
        | Node::(_T: _, k: any, v: any, left: any, right: any) => Node::(Black, k, v, left, right)
        | Empty::() => Empty::()  // 不应该发生
        | panic
};

// 查找函数
let lookup: lambda = cmp: lambda => tree: Tree(Any, Any) => key: any => {
    loop go: t: any = tree;
    match t
        | Empty::() => Nothing
        | Node::(_T: _, k: any, v: any, left: any, right: any) => {
            let cmp_result: int = cmp(key, k);
            if cmp_result < 0
                then go(left)
                else if cmp_result > 0
                    then go(right)
                    else Just(v)
        }
        | panic
};

// 检查键是否存在
let contains: lambda = cmp: lambda => tree: Tree(Any, Any) => key: any => {
    match lookup(cmp)(tree)(key)
        | Just::(_T: _) => true
        | Nothing::() => false
        | panic
};

// 获取树的大小
let size: lambda = tree: Tree(Any, Any) => {
    loop go: t: any = tree;
    match t
        | Empty::() => 0
        | Node::(_U: _, _V: _, _W: _, left: any, right: any) => 1 + go(left) + go(right)
        | panic
};

// 中序遍历
let inorder: lambda = tree: Tree(Any, Any) => f: lambda => {
    loop go: t: any = tree;
    match t
        | Empty::() => ()
        | Node::(_T: _, k: any, v: any, left: any, right: any) => {
            discard go(left);
            discard f(k, v);
            go(right)
        }
        | panic
};

// 导出所有公共接口
Red::Red &
Black::Black &
Color::Color &
Tree::Tree &
empty::empty &
insert::insert &
lookup::lookup &
contains::contains &
size::size &
inorder::inorder
