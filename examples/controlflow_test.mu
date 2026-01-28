let {
    return::(return: any)
} = import "lib/controlflow.mu";

let {
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = import "lib/string.mu";

let f: any = (a: nat, b: nat) => {
    let a2: nat = a * 2;
    let b2: nat = b * 3;
    @return a2 + b2;
    discard println("This line will never be executed");
};

let main: any = () => {
    let result: nat = f(3, 4);
    discard println("Result of f(3, 4): " + nat_to_string(result));
};

main()