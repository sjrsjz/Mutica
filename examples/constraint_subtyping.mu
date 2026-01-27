let lhs: any = constraint (a: nat, rec tail: [() | (b: nat | c: char, tail)]);
let rhs: any = constraint (x: nat, rec tail: [() | (y: nat | z: char | w: float, tail)]);
let true = lhs is sub rhs;
let false = rhs is sub lhs;
let true = lhs == lhs;
let true = rhs == rhs;

let lhs: any = exist (x, x) where x: nat;
let rhs: any = constraint (a: nat, b: nat);
let true = lhs is sub rhs;
let false = rhs is sub lhs;
println!("Ok")