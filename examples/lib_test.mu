let list_pkg: any = import "lib/list.mu";
let string_pkg: any = import "lib/string.mu";
let maybe_pkg: any = import "lib/maybe.mu";
let sort_pkg: any = import "lib/sort.mu";

// 创建测试列表: [1, 2, 3, 4, 5]
let test_list: any = list_pkg.cons(1, list_pkg.cons(2, list_pkg.cons(3, list_pkg.cons(4, list_pkg.cons(5, list_pkg.Nil)))));
let (_x: any, _x: any) = (test_list, @(1, 2, 3, 4, 5)); // 类型检查
let print_int_list: any = lst: list_pkg.List(int) -> {
    discard list_pkg.iter(lst)(x: int -> {
        discard print!(x);
        discard print!(' ');
    });
    discard print!('\n');
};

// 测试 filter: 过滤出大于 2 的元素，期望结果: [3, 4, 5]
discard print_int_list[list_pkg.filter(test_list)(x: int -> x > 2)];

// 测试 fold: 计算列表元素之和，期望结果: 15
discard println![list_pkg.fold(test_list)(0)((acc: int, x: int) -> acc + x)];

// 测试 foldr: 构建新列表，期望结果: [2, 4, 6, 8, 10]
discard print_int_list[list_pkg.foldr(test_list)(list_pkg.Nil)((h: int, acc: any) -> list_pkg.cons(h * 2, acc))];

// 测试 append: 连接两个列表，期望结果: [1, 2, 3, 4]
let list1: list_pkg.List(int) = list_pkg.cons(1, list_pkg.cons(2, list_pkg.Nil));
let list2: list_pkg.List(int) = list_pkg.cons(3, list_pkg.cons(4, list_pkg.Nil));
discard print_int_list[list_pkg.append(list1)(list2)];

// 测试 reverse: 反转列表，期望结果: [5, 4, 3, 2, 1]
discard print_int_list[list_pkg.reverse(test_list)];

// 测试 nth: 获取索引为 2 的元素，期望结果: 3
discard println![list_pkg.nth(test_list)(2)];

// 测试 take: 取前 3 个元素，期望结果: [1, 2, 3]
discard print_int_list[list_pkg.take(test_list)(3)];

// 测试 drop: 丢弃前 2 个元素，期望结果: [3, 4, 5]
discard print_int_list[list_pkg.drop(test_list)(2)];

// 测试 find: 查找第一个大于 3 的元素，期望结果: Just::4
discard println![list_pkg.find(test_list)(x: int -> x > 3)];

// 测试 find: 查找不存在的元素，期望结果: Nothing::()
discard println![list_pkg.find(test_list)(x: int -> x > 10)];

// 测试 list_all: 检查是否所有元素都大于 0，期望结果: true
discard println![list_pkg.list_all(test_list)(x: int -> x > 0)];

// 测试 list_all: 检查是否所有元素都大于 3，期望结果: false
discard println![list_pkg.list_all(test_list)(x: int -> x > 3)];

// 测试 list_any: 检查是否存在元素大于 4，期望结果: true
discard println![list_pkg.list_any(test_list)(x: int -> x > 4)];

// 测试 list_any: 检查是否存在元素小于 0，期望结果: false
discard println![list_pkg.list_any(test_list)(x: int -> x < 0)];

// 测试 map: 将所有元素乘以 2，期望结果: [2, 4, 6, 8, 10]
discard print_int_list[list_pkg.map(test_list)(x: int -> x * 2)];

// 测试 len: 获取列表长度，期望结果: 5
discard println![list_pkg.len(test_list)];

// 组合测试: filter + map + fold
// 找出所有偶数，乘以 3，然后求和，期望结果: (2 + 4) * 3 = 18
discard println![{
    let evens: any = list_pkg.filter(test_list)(x: int -> x % 2 == 0);
    let tripled: any = list_pkg.map(evens)(x: int -> x * 3);
    list_pkg.fold(tripled)(0)((acc: int, x: int) -> acc + x)
}];


discard maybe_pkg.unwrap {
    maybe_pkg.map(string_pkg.slice("Hello, Mutica!", 7, 13))(s: string_pkg.String -> {
        discard string_pkg.println(s); // 输出: Mutica
    })
};

discard println![string_pkg.String == list_pkg.List(char)];

discard println![sort_pkg.merge_sort(__less)(test_list)]; // 期望结果: [1, 2, 3, 4, 5]