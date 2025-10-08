let Option: any = T: any |-> (Some::T | None::());
let println: any = x: any |-> {
    discard print x;
    discard print '\n';
};
discard println[Option(1)];
discard println[Option(2)];
discard println[Option(int)];
Option(1), Option(2), Option(int), Option(1) <: Option(int), Option(2) <: Option(int), Option(1) <: Option(2)