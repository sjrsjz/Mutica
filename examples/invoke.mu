let constraint f: any = constraint x: nat => x + 1;
f(10) |> (constraint x: nat => x) // f x |> g => invoke<f, x, g>