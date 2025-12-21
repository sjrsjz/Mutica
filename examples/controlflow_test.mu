let constraint {
    return::(return: any)
} = import "lib/controlflow.mu";

let constraint {
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = import "lib/string.mu";

let constraint f: any = constraint (a: nat, b: nat) => {
    let constraint a2: nat = a * 2;
    let constraint b2: nat = b * 3;
    @return a2 + b2;
    discard println("This line will never be executed");
};

let constraint main: any = assert () => {
    let constraint result: nat = f(3, 4);
    discard println("Result of f(3, 4): " + nat_to_string(result));
};

main()