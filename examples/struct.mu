let Point: any = (x: int, y: int, z: int) => { x::x & y::y & z::z };
let p: any = Point(1, 2, 3);
let { x::(x: int) & z::(z: int) } = p;
p, x, z