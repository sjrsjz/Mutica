let {
    println::(println: lambda) &
    nat_to_string::(nat_to_string: lambda)
} = import "lib/string.mu";

let deref: lambda = mut T: any => T;

discard println("Simple `while` loop example:");
discard {
    let while: lambda = condition: lambda => body: lambda => {
        loop go: assert () = ();
        if condition() then {
            discard body();
            go()
        } else ()
    };

    let i: any = mut 0;
    discard while delay (deref i < 10) delay {
        discard println!(deref i);
        i := deref i + 1
    };
    discard println("Final i: " + nat_to_string(deref i));
};

discard println("Mutable variable example:");
discard {

    let mut_a: any = mut 1;
    let alias: any = mut_a;
    discard println! mut_a;
    discard println!(deref mut_a);
    let mut const_val: any = mut_a;
    discard mut_a := deref mut_a + 1;
    discard println!(deref mut_a);
    discard println!(deref alias);
    discard println!(const_val);
};


discard println("Counter example:");
discard {
    let counter: any = mut 0;
    discard {
        loop go: i: nat = 0;
        if i < 500 then {
            discard counter := deref counter + i;
            go(i + 1)
        } else ()

    };
    discard println!(deref counter);
};
