let {
    String::(String: any) &
    println::(println: any) &
    nat_to_string::(nat_to_string: any) &
    print::(print: any)
} = import "lib/string.mu";

let debug_let: any = f: any => v: any => {
    discard print("Debug let:");
    discard print! v;
    discard print! '\n';
    f(v)
};

@debug_let a: nat = 42;
@debug_let b: nat = a + 1;
@debug_let c: nat = b * 2;
@debug_let s: String = nat_to_string(c);
println("Final result: " + s)