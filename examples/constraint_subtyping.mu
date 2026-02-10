let lhs: any = constraint (a: nat, rec tail: [() | (b: nat | c: char, tail)]);
let rhs: any = constraint (x: nat, rec tail: [() | (y: nat | z: char | w: float, tail)]);
let true = lhs is sub rhs;
let false = rhs is sub lhs;
let true = lhs == lhs;
let true = rhs == rhs;

let lhs: any = exist (x: any, x: any) where x: nat;
let rhs: any = exist (a: any, b: any) where {a: nat, b: nat};
let true = lhs is sub rhs;
let false = rhs is sub lhs;
println!("Ok")