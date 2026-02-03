let {
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = import "lib/string.mu";

let deref: any = mut T: any => T;

println("Simple `while` loop example:");
{
    let while: any = condition: any => body: any => {
        loop go: assert () = ();
        if condition() then {
            body();
            go()
        } else ()
    };

    let i: any = mut 0;
    while delay (deref i < 10) delay {
        println!(deref i);
        i := deref i + 1
    };
    println("Final i: " + nat_to_string(deref i));
};

println("Mutable variable example:");
{

    let mut_a: any = mut 1;
    let alias: any = mut_a;
    println! mut_a;
    println!(deref mut_a);
    let mut const_val: any = mut_a;
    mut_a := deref mut_a + 1;
    println!(deref mut_a);
    println!(deref alias);
    println!(const_val);
};


println("Counter example:");
{
    let counter: any = mut 0;
    {
        loop go: i: nat = 0;
        if i < 500 then {
            counter := deref counter + i;
            go(i + 1)
        } else ()

    };
    println!(deref counter);
};
