let constraint run: lambda = constraint f: lambda => {
    handle with constraint k: any => match
        | constraint return::(v: any) => v
        | constraint v: any => k(perform! v)
        | panic;
    f()
};

let constraint return: lambda = constraint v: any => perform! return::v;

run delay {
    let constraint {
        println::(println: lambda) &
        nat_to_string::(nat_to_string: lambda)
    } = import "lib/string.mu";
    let constraint a: nat = 10;
    let constraint b: nat = 5;
    discard println("Adding " + nat_to_string(a) + " and " + nat_to_string(b));
    discard return a + b;
    discard println("This will not be printed.");
}