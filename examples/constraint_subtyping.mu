let lhs: any = constraint (a: nat, rec tail: [() | (b: nat | c: nat, tail)]);
let rhs: any = constraint (x: nat, rec tail: [() | (y: nat, tail)]);
lhs is sub rhs