let rbtree_pkg: any = import "lib/rbtree.mu";
let string_pkg: any = import "lib/string.mu";
let {
    insert::(insert: any) &
    lookup::(lookup: any) &
    inorder::(inorder: any) &
    size::(size: any) &
    empty::(empty: any)
} = rbtree_pkg;
let {
    println::(println: any) &
    print::(print: any)
} = string_pkg;

// 整数比较函数
let int_cmp: any = (a: int, b: int) => {
    if a < b
        then -1
        else if a > b
            then 1
            else 0
};

// 测试红黑树
let main: any = () => {
    // 创建空树
    let tree: any = empty;
    
    // 插入一些键值对
    discard println("插入元素: 10, 5, 15, 3, 7, 12, 17");
    let tree1: any = insert(int_cmp)(tree)(10)("十");
    let tree2: any = insert(int_cmp)(tree1)(5)("五");
    let tree3: any = insert(int_cmp)(tree2)(15)("十五");
    let tree4: any = insert(int_cmp)(tree3)(3)("三");
    let tree5: any = insert(int_cmp)(tree4)(7)("七");
    let tree6: any = insert(int_cmp)(tree5)(12)("十二");
    let tree7: any = insert(int_cmp)(tree6)(17)("十七");
    
    // 打印树的大小
    discard print("树的大小: ");
    discard print!(size(tree7));
    discard print!('\n');
    
    // 查找操作
    discard print("查找键 7:");
    let result7: any = lookup(int_cmp)(tree7)(7);
    discard match result7
        | Just::(v: any) => {
            discard print("找到: ");
            println(v)
        }
        | Nothing::() => println("未找到")
        | panic;
    
    // 查找不存在的键
    discard println("查找键 20:");
    let result20: any = lookup(int_cmp)(tree7)(20);
    discard match result20
        | Just::(v: any) => {
            discard print("找到: ");
            println(v)
        }
        | Nothing::() => println("未找到")
        | panic;
    
    // 中序遍历 (应该按升序输出)
    discard println("中序遍历:");
    discard inorder(tree7)((k: int, v: any) => {
        discard print("  键: ");
        discard print!(k);
        discard print(", 值: ");
        println(v)
    });
    
    ()
};

main()
