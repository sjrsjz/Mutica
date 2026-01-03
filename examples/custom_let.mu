let constraint {
    String::(String: any) &
    println::(println: lambda) &
    nat_to_string::(nat_to_string: lambda) &
    print::(print: lambda)
} = import "lib/string.mu";

let constraint debug_let: lambda = constraint f: lambda => constraint v: any => {
    discard print("Debug let:");
    discard print! v;
    discard print! '\n';
    f(v)
};

debug_let constraint a: nat = 42;
debug_let constraint b: nat = a + 1;
debug_let constraint c: nat = b * 2;
debug_let constraint s: String = nat_to_string(c);
println("Final result: " + s)