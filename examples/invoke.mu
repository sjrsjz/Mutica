let f: any = (x: nat) => x + 1;
f(10) |> (x: nat => x) // f x |> g => invoke<f, x, g>