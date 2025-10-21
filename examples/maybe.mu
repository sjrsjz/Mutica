let pkg: any = import "lib/maybe.mu";
let v1: any = pkg.Just(41);
let v2: any = pkg.Nothing;
pkg.map(v1)(x: int -> x + 1), pkg.map(v2)(x: int -> x + 1)