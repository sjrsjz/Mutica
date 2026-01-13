let vec3: any = (float, float, float);

extend $"op#add": (A: vec3, B: vec3) => {
    let (x1: float, y1: float, z1: float) = A;
    let (x2: float, y2: float, z2: float) = B;
    (x1 + x2, y1 + y2, z1 + z2)
};

extend $"op#sub": (A: vec3, B: vec3) => {
    let (x1: float, y1: float, z1: float) = A;
    let (x2: float, y2: float, z2: float) = B;
    (x1 - x2, y1 - y2, z1 - z2)
};

extend $"op#mul": (A: vec3, B: vec3) => {
    let (x1: float, y1: float, z1: float) = A;
    let (x2: float, y2: float, z2: float) = B;
    (x1 * x2, y1 * y2, z1 * z2)
};

extend $"op#mul": (A: vec3, scale: float) => {
    let (x1: float, y1: float, z1: float) = A;
    (x1 * scale, y1 * scale, z1 * scale)
};

extend $"op#div": (A: vec3, scale: float) => {
    let (x1: float, y1: float, z1: float) = A;
    (x1 / scale, y1 / scale, z1 / scale)
};

let dot: any = (A: vec3, B: vec3) => {
    let (x1: float, y1: float, z1: float) = A;
    let (x2: float, y2: float, z2: float) = B;
    x1 * x2 + y1 * y2 + z1 * z2
};

let cross: any = (A: vec3, B: vec3) => {
    let (x1: float, y1: float, z1: float) = A;
    let (x2: float, y2: float, z2: float) = B;
    (
        y1 * z2 - z1 * y2,
        z1 * x2 - x1 * z2,
        x1 * y2 - y1 * x2
    )
};

let A: vec3 = (1.0, 2.0, 3.0);
let B: vec3 = (4.0, 5.0, 6.0);
discard println!(A + B);
discard println!(A - B);
discard println!(A * B);
discard println!(A / 2.0);
discard println!(dot(A, B));
discard println!(cross(A, B));