let constraint Point: any = constraint (x: nat, y: nat, z: nat) => { x::x & y::y & z::z };
let constraint p: any = Point(1, 2, 3);
let constraint { x::(x: nat) & z::(z: nat) } = p;
p, x, z