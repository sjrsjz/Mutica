let constraint String::(String: any) = import "lib/string.mu";
let constraint Option: lambda = constraint T: any => (Some::T | None::());
let constraint println: lambda = constraint x: any => {
    discard println! x;
};
discard println[Option(1)];
discard println[Option(2)];
discard println[Option(nat)];
Option(1), Option(2), Option(nat), Option(nat) is sub [Option(nat | String)], Option(2) is sub [Option(nat)], Option(1) is Option(2)