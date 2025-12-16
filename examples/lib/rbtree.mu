let constraint maybe_pkg: any = import "maybe.mu";
let constraint {
    int::(int: any) &
    Lt::($"op#lt": any) &
    Gt::($"op#gt": any)
} = import "int.mu";
let constraint Just::(Just: any) = maybe_pkg;
let constraint Nothing::(Nothing: any) = maybe_pkg;

// 颜色定义
let constraint Red: any = Red::();
let constraint Black: any = Black::();
let constraint Color: any = (Red::() | Black::());

// 红黑树定义
// Tree: Empty | Node(color, key, value, left, right)
let constraint Tree: any = constraint K: any => constraint V: any => rec tree: (
    Empty::() | 
    Node::(Color, K, V, tree, tree)
);

// 创建空树
let constraint empty: any = Empty::();

// 平衡函数 - 处理红黑树的4种违规情况
let constraint balance: any = constraint t: any => 
    match t
        // 情况1: 左-左红红
        | constraint Node::(Black, z: any, zv: any, Node::(Red, y: any, yv: any, Node::(Red, x: any, xv: any, a: any, b: any), c: any), d: any) =>
            Node::(Red, y, yv, Node::(Black, x, xv, a, b), Node::(Black, z, zv, c, d))
        // 情况2: 左-右红红
        | constraint Node::(Black, z: any, zv: any, Node::(Red, x: any, xv: any, a: any, Node::(Red, y: any, yv: any, b: any, c: any)), d: any) =>
            Node::(Red, y, yv, Node::(Black, x, xv, a, b), Node::(Black, z, zv, c, d))
        // 情况3: 右-左红红
        | constraint Node::(Black, x: any, xv: any, a: any, Node::(Red, z: any, zv: any, Node::(Red, y: any, yv: any, b: any, c: any), d: any)) =>
            Node::(Red, y, yv, Node::(Black, x, xv, a, b), Node::(Black, z, zv, c, d))
        // 情况4: 右-右红红
        | constraint Node::(Black, x: any, xv: any, a: any, Node::(Red, y: any, yv: any, b: any, Node::(Red, z: any, zv: any, c: any, d: any))) =>
            Node::(Red, y, yv, Node::(Black, x, xv, a, b), Node::(Black, z, zv, c, d))
        // 其他情况保持不变
        | constraint tree: any => tree
        | panic;

// 插入辅助函数
let constraint insert_helper: any = constraint cmp: any => constraint tree: any => constraint key: any => constraint value: any => {
    loop go: constraint t: any = tree;
    match t
        | assert Empty::() => Node::(Red, key, value, Empty::(), Empty::())
        | constraint Node::(color: any, k: any, v: any, left: any, right: any) => {
            let constraint cmp_result: int = cmp(key, k);
            if cmp_result < 0
                then balance(Node::(color, k, v, go(left), right))
                else if cmp_result > 0
                    then balance(Node::(color, k, v, left, go(right)))
                    else Node::(color, key, value, left, right)  // 更新值
        }
        | panic
};

// 插入函数 - 确保根节点是黑色
let constraint insert: any = constraint cmp: any => constraint tree: any => constraint key: any => constraint value: any => {
    let constraint result: any = insert_helper(cmp)(tree)(key)(value);
    match result
        | constraint Node::(_T: _, k: any, v: any, left: any, right: any) => Node::(Black, k, v, left, right)
        | assert Empty::() => Empty::()  // 不应该发生
        | panic
};

// 查找函数
let constraint lookup: any = constraint cmp: any => constraint tree: any => constraint key: any => {
    loop go: constraint t: any = tree;
    match t
        | assert Empty::() => Nothing
        | constraint Node::(_T: _, k: any, v: any, left: any, right: any) => {
            let constraint cmp_result: int = cmp(key, k);
            if cmp_result < 0
                then go(left)
                else if cmp_result > 0
                    then go(right)
                    else Just(v)
        }
        | panic
};

// 检查键是否存在
let constraint contains: any = constraint cmp: any => constraint tree: any => constraint key: any => {
    match lookup(cmp)(tree)(key)
        | constraint Just::(_T: _) => true
        | assert Nothing::() => false
        | panic
};

// 获取树的大小
let constraint size: any = constraint tree: any => {
    loop go: constraint t: any = tree;
    match t
        | assert Empty::() => 0
        | constraint Node::(_U: _, _V: _, _W: _, left: any, right: any) => 1 + go(left) + go(right)
        | panic
};

// 中序遍历
let constraint inorder: any = constraint tree: any => constraint f: any => {
    loop go: constraint t: any = tree;
    match t
        | assert Empty::() => ()
        | constraint Node::(_T: _, k: any, v: any, left: any, right: any) => {
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
