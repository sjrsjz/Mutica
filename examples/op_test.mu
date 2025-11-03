// 1 + 1, 2 * 2, 3 - 4, 6 / 2, 7 % 3, - 5,
// 1 < 2, 2 <= 2, 3 > 2, 4 >= 5
[match | (x: any, x: any) => x | (x: any, y: any) => print!(x, y) | panic](3,2)