let a: any = never;
let b: any = unknown;
let c: any = 'C';
a is sub a, a is sub b, b is sub a, b is sub b, c is sub a, c is sub b, a is sub c, b is sub c, a & c, a | c, b & c, b | c