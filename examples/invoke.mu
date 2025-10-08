let f: any = (x: int) |-> x + 1;
f(10) |> (x: int |-> x) // f x |> g => invoke<f, x, g>