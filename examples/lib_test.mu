let list_pkg: any = import "lib/list.mu";
let string_pkg: any = import "lib/string.mu";
let maybe_pkg: any = import "lib/maybe.mu";
let sort_pkg: any = import "lib/sort.mu";

let {
    List::(List: any) &
    Nil::(Nil: any) &
    cons::(cons: any) &
    filter::(filter: any) &
    iter::(iter: any) &
    fold::(fold: any) &
    foldr::(foldr: any) &
    append::(append: any) &
    reverse::(reverse: any) &
    nth::(nth: any) &
    take::(take: any) &
    drop::(drop: any) &
    find::(find: any) &
    allof::(allof: any) &
    anyof::(anyof: any) &
    map::(list_map: any) &
    len::(len: any)
} = list_pkg;

let {
    unwrap::(unwrap: any) &
    map::(maybe_map: any)
} = maybe_pkg;

let {
    slice::(slice: any) &
    String::(String: any) &
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = string_pkg;

let {
    merge_sort::(merge_sort: any) &
    quick_sort::(quick_sort: any) &
    insert_sort::(insert_sort: any)
} = sort_pkg;

// 创建测试列表: [1, 2, 3, 4, 5]
let test_list: any = cons(1, cons(2, cons(3, cons(4, cons(5, Nil)))));
let true = test_list == (1, 2, 3, 4, 5); // 类型检查
let print_int_list: any = lst: List(nat) => {
    @iter x: nat = lst in {
        print!(x);
        print!(' ');
    };
    print!('\n');
};

// 测试 filter: 过滤出大于 2 的元素，期望结果: [3, 4, 5]
print_int_list[filter(test_list)(x: nat => x > 2)];

// 测试 fold: 计算列表元素之和，期望结果: 15
println![fold(test_list)(0)((acc: nat, x: nat) => acc + x)];

// 测试 foldr: 构建新列表，期望结果: [2, 4, 6, 8, 10]
print_int_list[foldr(test_list)(Nil)((h: nat, acc: any) => cons(h * 2, acc))];

// 测试 append: 连接两个列表，期望结果: [1, 2, 3, 4]
let list1: List(nat) = cons(1, cons(2, Nil));
let list2: List(nat) = cons(3, cons(4, Nil));
print_int_list[list1.append(list2)];

// 测试 reverse: 反转列表，期望结果: [5, 4, 3, 2, 1]
print_int_list[reverse(test_list)];

// 测试 nth: 获取索引为 2 的元素，期望结果: 3
println![test_list.nth(2)];

// 测试 take: 取前 3 个元素，期望结果: [1, 2, 3]
print_int_list[test_list.take(3)];

// 测试 drop: 丢弃前 2 个元素，期望结果: [3, 4, 5]
print_int_list[test_list.drop(2)];

// 测试 find: 查找第一个大于 3 的元素，期望结果: Just::4
println![test_list.find(x: nat => x > 3)];

// 测试 find: 查找不存在的元素，期望结果: Nothing::()
println![test_list.find(x: nat => x > 10)];

// 测试 allof: 检查是否所有元素都大于 0，期望结果: true
println![test_list.allof(x: nat => x > 0)];

// 测试 allof: 检查是否所有元素都大于 3，期望结果: false
println![test_list.allof(x: nat => x > 3)];

// 测试 anyof: 检查是否存在元素大于 4，期望结果: true
println![test_list.anyof(x: nat => x > 4)];

// 测试 anyof: 检查是否存在元素小于 0，期望结果: false
println![test_list.anyof(x: nat => x < 0)];

// 测试 map: 将所有元素乘以 2，期望结果: [2, 4, 6, 8, 10]
print_int_list[test_list.list_map(x: nat => x * 2)];

// 测试 len: 获取列表长度，期望结果: 5
println![len(test_list)];

// 组合测试: filter + map + fold
// 找出所有偶数，乘以 3，然后求和，期望结果: (2 + 4) * 3 = 18
println![{
    let evens: any = filter(test_list)(x: nat => x % 2 == 0);
    let tripled: any = list_map(evens)(x: nat => x * 3);
    fold(tripled)(0)((acc: nat, x: nat) => acc + x)
}];


unwrap {
    maybe_map(slice("Hello, Mutica!", 7, 13))(s: String => {
        println(s); // 输出: Mutica
    })
};

println![String == List(char)];

println![$"op#lt".merge_sort(test_list)]; // 期望结果: [1, 2, 3, 4, 5]
println![$"op#lt".quick_sort(test_list)]; // 期望结果: [1, 2, 3, 4, 5]
println![$"op#lt".insert_sort(test_list)]; // 期望结果: [1, 2, 3, 4, 5]

1234567890.nat_to_string.println; // 输出: "1234567890"