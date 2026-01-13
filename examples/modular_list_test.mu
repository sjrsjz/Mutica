let list_pkg: any = import "lib/list.mu";

let {
    Modular::(Modular: any) &
    Greater::(Greater: any) &
    Range::(Range: any) &
    Exact::(Exact: any)
} = list_pkg;
discard println!(Modular(nat, 3, 1));

// 测试 Modular(nat, 3, 1) - 长度为 1, 4, 7, 10, ...
let mod3_1_len1: any = (42,);
let mod3_1_len4: any = (1, 2, 3, 4);
let mod3_1_len7: any = (1, 2, 3, 4, 5, 6, 7);

// 测试 Modular(nat, 2, 0) - 偶数长度 (0, 2, 4, 6, ...)
let even_len0: any = ();
let even_len2: any = (10, 20);
let even_len4: any = (1, 2, 3, 4);

// 测试 Modular(nat, 2, 1) - 奇数长度 (1, 3, 5, 7, ...)
let odd_len1: any = (99,);
let odd_len3: any = (1, 2, 3);
let odd_len5: any = (1, 2, 3, 4, 5);

// 测试 Greater(nat, 3) - 至少 3 个元素
let greater3: any = (1, 2, 3, 4);

// 测试 Range(nat, 2, 5) - 长度在 2 到 5 之间
let range2_5: any = (1, 2, 3);

// 测试 Exact(nat, 4) - 恰好 4 个元素
let exact4: any = (1, 2, 3, 4);

// 验证类型
let check1: Modular(nat, 3, 1) = mod3_1_len1;
let check2: Modular(nat, 3, 1) = mod3_1_len4;
let check3: Modular(nat, 3, 1) = mod3_1_len7;

let check4: Modular(nat, 2, 0) = even_len0;
let check5: Modular(nat, 2, 0) = even_len2;
let check6: Modular(nat, 2, 0) = even_len4;

let check7: Modular(nat, 2, 1) = odd_len1;
let check8: Modular(nat, 2, 1) = odd_len3;
let check9: Modular(nat, 2, 1) = odd_len5;

let check10: Greater(nat, 3) = greater3;
let check11: Range(nat, 2, 5) = range2_5;
let check12: Exact(nat, 4) = exact4;

discard println!(check1);
discard println!(check2);
discard println!(check3);
discard println!(check4);
discard println!(check5);
discard println!(check6);
discard println!(check7);
discard println!(check8);
discard println!(check9);
discard println!(check10);
discard println!(check11);
discard println!(check12);