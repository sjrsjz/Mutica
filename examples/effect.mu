let run: any = f: any => {
    handle with dyn_rec handler: k: any => match
        | return::(v: any) => v
        | v: any => {
            let result: any = perform! v;
            handle with handler;
            k result
        }
        | panic;
    f()
};

let return: any = v: any => perform! return::v;

run delay {
    let {
        println::(println: any) &
        nat_to_string::(nat_to_string: any)
    } = import "lib/string.mu";
    let a: nat = 10;
    let b: nat = 5;
    discard println("Adding " + nat_to_string(a) + " and " + nat_to_string(b));
    discard return a + b;
    discard println("This will not be printed.");
}