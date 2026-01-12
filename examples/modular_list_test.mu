let constraint list_pkg: any = import "lib/list.mu";

let constraint {
    Modular::(Modular: any) &
    Greater::(Greater: any) &
    Range::(Range: any) &
    Exact::(Exact: any)
} = list_pkg;
discard println!(Modular(nat, 3, 1));

// 测试 Modular(nat, 3, 1) - 长度为 1, 4, 7, 10, ...
let constraint mod3_1_len1: any = (42,);
let constraint mod3_1_len4: any = (1, 2, 3, 4);
let constraint mod3_1_len7: any = (1, 2, 3, 4, 5, 6, 7);

// 测试 Modular(nat, 2, 0) - 偶数长度 (0, 2, 4, 6, ...)
let constraint even_len0: any = ();
let constraint even_len2: any = (10, 20);
let constraint even_len4: any = (1, 2, 3, 4);

// 测试 Modular(nat, 2, 1) - 奇数长度 (1, 3, 5, 7, ...)
let constraint odd_len1: any = (99,);
let constraint odd_len3: any = (1, 2, 3);
let constraint odd_len5: any = (1, 2, 3, 4, 5);

// 测试 Greater(nat, 3) - 至少 3 个元素
let constraint greater3: any = (1, 2, 3, 4);

// 测试 Range(nat, 2, 5) - 长度在 2 到 5 之间
let constraint range2_5: any = (1, 2, 3);

// 测试 Exact(nat, 4) - 恰好 4 个元素
let constraint exact4: any = (1, 2, 3, 4);

// 验证类型
let constraint check1: Modular(nat, 3, 1) = mod3_1_len1;
let constraint check2: Modular(nat, 3, 1) = mod3_1_len4;
let constraint check3: Modular(nat, 3, 1) = mod3_1_len7;

let constraint check4: Modular(nat, 2, 0) = even_len0;
let constraint check5: Modular(nat, 2, 0) = even_len2;
let constraint check6: Modular(nat, 2, 0) = even_len4;

let constraint check7: Modular(nat, 2, 1) = odd_len1;
let constraint check8: Modular(nat, 2, 1) = odd_len3;
let constraint check9: Modular(nat, 2, 1) = odd_len5;

let constraint check10: Greater(nat, 3) = greater3;
let constraint check11: Range(nat, 2, 5) = range2_5;
let constraint check12: Exact(nat, 4) = exact4;

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