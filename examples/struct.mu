let Point: any = (x: nat, y: nat, z: nat) => { x::x & y::y & z::z };
let p: any = Point(1, 2, 3);
let { x::(x: nat) & z::(z: nat) } = p;
p, x, z