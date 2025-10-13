let pkg: any = import "lib/list.mu";

// 创建测试列表: [1, 2, 3, 4, 5]
let test_list: any = pkg.cons(1, pkg.cons(2, pkg.cons(3, pkg.cons(4, pkg.cons(5, pkg.Nil)))));

// 测试 filter: 过滤出大于 2 的元素，期望结果: [3, 4, 5]
discard println![pkg.filter(test_list)(x: int |-> x > 2)];

// 测试 fold: 计算列表元素之和，期望结果: 15
let sum: any = pkg.fold(test_list)(0)(acc: int |-> x: int |-> acc + x);

// 测试 foldr: 构建新列表，期望结果: [2, 4, 6, 8, 10]
discard println![pkg.foldr(test_list)(pkg.Nil)(h: int |-> acc: any |-> pkg.cons(h * 2, acc))];

// 测试 append: 连接两个列表，期望结果: [1, 2, 3, 4]
let list1: any = pkg.cons(1, pkg.cons(2, pkg.Nil));
let list2: any = pkg.cons(3, pkg.cons(4, pkg.Nil));
discard pkg.append(list1)(list2);

// 测试 reverse: 反转列表，期望结果: [5, 4, 3, 2, 1]
discard pkg.reverse(test_list);

// 测试 nth: 获取索引为 2 的元素，期望结果: 3
let third_elem: any = pkg.nth(test_list)(2);

// 测试 take: 取前 3 个元素，期望结果: [1, 2, 3]
discard pkg.take(test_list)(3);

// 测试 drop: 丢弃前 2 个元素，期望结果: [3, 4, 5]
discard pkg.drop(test_list)(2);

// 测试 find: 查找第一个大于 3 的元素，期望结果: Just::4
discard pkg.find(test_list)(x: int |-> x > 3);

// 测试 find: 查找不存在的元素，期望结果: Nothing::()
discard pkg.find(test_list)(x: int |-> x > 10);

// 测试 list_all: 检查是否所有元素都大于 0，期望结果: true
let all_positive: any = pkg.list_all(test_list)(x: int |-> x > 0);

// 测试 list_all: 检查是否所有元素都大于 3，期望结果: false
let all_gt_3: any = pkg.list_all(test_list)(x: int |-> x > 3);

// 测试 list_any: 检查是否存在元素大于 4，期望结果: true
let any_gt_4: any = pkg.list_any(test_list)(x: int |-> x > 4);

// 测试 list_any: 检查是否存在元素小于 0，期望结果: false
let any_negative: any = pkg.list_any(test_list)(x: int |-> x < 0);

// 测试 map (原有功能): 将所有元素乘以 2，期望结果: [2, 4, 6, 8, 10]
discard pkg.map(test_list)(x: int |-> x * 2);

// 测试 len (原有功能): 获取列表长度，期望结果: 5
let length: any = pkg.len(test_list);

// 组合测试: filter + map + fold
// 找出所有偶数，乘以 3，然后求和，期望结果: (2 + 4) * 3 = 18
let combo_result: any = {
    let evens: any = pkg.filter(test_list)(x: int |-> x % 2 == 0);
    let tripled: any = pkg.map(evens)(x: int |-> x * 3);
    pkg.fold(tripled)(0)(acc: int |-> x: int |-> acc + x)
};

// 输出主要测试结果
(
    sum,
    third_elem,
    length,
    all_positive,
    all_gt_3,
    any_gt_4,
    any_negative,
    combo_result
)
