handle {
    let x: int = 1;
    let y: int = 2 / x;
    1 + (perform! x) + (perform! x)
}
with
    | (int, int) => 42
    | int => 43
    | panic