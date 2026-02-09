let foo: any = match
    | Vec::() => Vec::()
    | exist Vec::(x: any ~ _y: any) where {_y: sub (!..x)} => Vec::(!..x)
    | v: Vec::any => panic_with!(v)
    | v: any => v
    | panic;

println![foo()];
println![foo(1, 1)];
println![foo(char, char, char)];
println![foo(char, char, nat)];
println![foo(typeof Vec::"Hello, world!")];