let run: lambda = f: lambda => {
    handle with k: any => match
        | return::(v: any) => v
        | v: any => k(perform! v)
        | panic;
    f()
};

let return: lambda = v: any => perform! return::v;

run delay {
    let {
        println::(println: lambda) &
        nat_to_string::(nat_to_string: lambda)
    } = import "lib/string.mu";
    let a: nat = 10;
    let b: nat = 5;
    discard println("Adding " + nat_to_string(a) + " and " + nat_to_string(b));
    discard return a + b;
    discard println("This will not be printed.");
}