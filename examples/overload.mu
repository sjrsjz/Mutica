let constraint vec3: any = (float, float, float);

extend $"op#add": constraint (A: vec3, B: vec3) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    let constraint (x2: float, y2: float, z2: float) = B;
    (x1 + x2, y1 + y2, z1 + z2)
};

extend $"op#sub": constraint (A: vec3, B: vec3) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    let constraint (x2: float, y2: float, z2: float) = B;
    (x1 - x2, y1 - y2, z1 - z2)
};

extend $"op#mul": constraint (A: vec3, B: vec3) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    let constraint (x2: float, y2: float, z2: float) = B;
    (x1 * x2, y1 * y2, z1 * z2)
};

extend $"op#mul": constraint (A: vec3, scale: float) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    (x1 * scale, y1 * scale, z1 * scale)
};

extend $"op#div": constraint (A: vec3, scale: float) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    (x1 / scale, y1 / scale, z1 / scale)
};

let constraint dot: any = constraint (A: vec3, B: vec3) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    let constraint (x2: float, y2: float, z2: float) = B;
    x1 * x2 + y1 * y2 + z1 * z2
};

let constraint cross: any = constraint (A: vec3, B: vec3) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    let constraint (x2: float, y2: float, z2: float) = B;
    (
        y1 * z2 - z1 * y2,
        z1 * x2 - x1 * z2,
        x1 * y2 - y1 * x2
    )
};

let constraint A: vec3 = (1.0, 2.0, 3.0);
let constraint B: vec3 = (4.0, 5.0, 6.0);
discard println!(A + B);
discard println!(A - B);
discard println!(A * B);
discard println!(A / 2.0);
discard println!(dot(A, B));
discard println!(cross(A, B));