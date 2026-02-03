let String::(String: any) = import "lib/string.mu";
let Option: any = T: any => (Some::T | None::());
let println: any = x: any => {
    println! x;
};
println[Option(1)];
println[Option(2)];
println[Option(nat)];
Option(1), Option(2), Option(nat), Option(nat) is sub [Option(nat | String)], Option(2) is sub [Option(nat)], Option(1) is Option(2)