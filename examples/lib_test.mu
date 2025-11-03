let list_pkg: any = import "lib/list.mu";
let string_pkg: any = import "lib/string.mu";
let maybe_pkg: any = import "lib/maybe.mu";
let sort_pkg: any = import "lib/sort.mu";

let List::(List: any) = list_pkg;
let Nil::(Nil: any) = list_pkg;
let String::(String: any) = string_pkg;

let cons::(cons: any) = list_pkg;
let filter::(filter: any) = list_pkg;
let iter::(iter: any) = list_pkg;
let fold::(fold: any) = list_pkg;
let foldr::(foldr: any) = list_pkg;
let append::(append: any) = list_pkg;
let reverse::(reverse: any) = list_pkg;
let nth::(nth: any) = list_pkg;
let take::(take: any) = list_pkg;
let drop::(drop: any) = list_pkg;
let find::(find: any) = list_pkg;
let list_all::(list_all: any) = list_pkg;
let list_any::(list_any: any) = list_pkg;
let map::(list_map: any) = list_pkg;
let len::(len: any) = list_pkg;

let unwrap::(unwrap: any) = maybe_pkg;
let map::(maybe_map: any) = maybe_pkg;

let slice::(slice: any) = string_pkg;
let println::(println: any) = string_pkg;

let merge_sort::(merge_sort: any) = sort_pkg;


// 创建测试列表: [1, 2, 3, 4, 5]
let test_list: any = cons(1, cons(2, cons(3, cons(4, cons(5, Nil)))));
let (_x: any, _x: any) = (test_list, (1, 2, 3, 4, 5)); // 类型检查
let print_int_list: any = lst: List(int) => {
    discard iter(lst)(x: int => {
        discard print!(x);
        discard print!(' ');
    });
    discard print!('\n');
};

// 测试 filter: 过滤出大于 2 的元素，期望结果: [3, 4, 5]
discard print_int_list[filter(test_list)(x: int => x > 2)];

// 测试 fold: 计算列表元素之和，期望结果: 15
discard println![fold(test_list)(0)((acc: int, x: int) => acc + x)];

// 测试 foldr: 构建新列表，期望结果: [2, 4, 6, 8, 10]
discard print_int_list[foldr(test_list)(Nil)((h: int, acc: any) => cons(h * 2, acc))];

// 测试 append: 连接两个列表，期望结果: [1, 2, 3, 4]
let list1: List(int) = cons(1, cons(2, Nil));
let list2: List(int) = cons(3, cons(4, Nil));
discard print_int_list[append(list1)(list2)];

// 测试 reverse: 反转列表，期望结果: [5, 4, 3, 2, 1]
discard print_int_list[reverse(test_list)];

// 测试 nth: 获取索引为 2 的元素，期望结果: 3
discard println![nth(test_list)(2)];

// 测试 take: 取前 3 个元素，期望结果: [1, 2, 3]
discard print_int_list[take(test_list)(3)];

// 测试 drop: 丢弃前 2 个元素，期望结果: [3, 4, 5]
discard print_int_list[drop(test_list)(2)];

// 测试 find: 查找第一个大于 3 的元素，期望结果: Just::4
discard println![find(test_list)(x: int => x > 3)];

// 测试 find: 查找不存在的元素，期望结果: Nothing::()
discard println![find(test_list)(x: int => x > 10)];

// 测试 list_all: 检查是否所有元素都大于 0，期望结果: true
discard println![list_all(test_list)(x: int => x > 0)];

// 测试 list_all: 检查是否所有元素都大于 3，期望结果: false
discard println![list_all(test_list)(x: int => x > 3)];

// 测试 list_any: 检查是否存在元素大于 4，期望结果: true
discard println![list_any(test_list)(x: int => x > 4)];

// 测试 list_any: 检查是否存在元素小于 0，期望结果: false
discard println![list_any(test_list)(x: int => x < 0)];

// 测试 map: 将所有元素乘以 2，期望结果: [2, 4, 6, 8, 10]
discard print_int_list[list_map(test_list)(x: int => x * 2)];

// 测试 len: 获取列表长度，期望结果: 5
discard println![len(test_list)];

// 组合测试: filter + map + fold
// 找出所有偶数，乘以 3，然后求和，期望结果: (2 + 4) * 3 = 18
discard println![{
    let evens: any = filter(test_list)(x: int => x % 2 == 0);
    let tripled: any = list_map(evens)(x: int => x * 3);
    fold(tripled)(0)((acc: int, x: int) => acc + x)
}];


discard unwrap {
    maybe_map(slice("Hello, Mutica!", 7, 13))(s: String => {
        discard println(s); // 输出: Mutica
    })
};

discard println![String == List(char)];

discard println![merge_sort((x: any, y: any) => __less!(x, y, true, false))(test_list)]; // 期望结果: [1, 2, 3, 4, 5]