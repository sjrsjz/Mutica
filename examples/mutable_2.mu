let {
    println::(println: any) &
    String::(String: any)
} = import "lib/string.mu";

let mutable: any = k: any => (unique_name: String, init_value: any ~ on_change: [(lambda | v: any | panic,) | ()]) => {
    let handler: any = dyn_rec self: state: any => {
        dyn_rec built_handler: k: any => match
            | get::Mutable::unique_name => {
                handle with built_handler;
                k(state)
            }
            | set::(Mutable::unique_name, new_state: any) => {
                handle with (self new_state);
                match on_change
                    | (f: (lambda | v: any | panic),) => f(Mutable::unique_name)
                    | () => ()
                    | panic;
                k()
            }
            | v: any => {
                let result: any = perform! v;
                handle with built_handler;
                k result
            }
            | panic
    };
    handle with handler init_value;
    k(Mutable::unique_name)
};

let Mut: any = Mutable::String;

// let get: any = unique_name: String => perform! get::unique_name;
// let set: any = unique_name: String => new_value: any => perform! set::(unique_name, new_value);
extend $"op#assign": (unique_name: Mut, new_value: any) => perform! set::(unique_name, new_value);
extend $"op#not": unique_name: Mut => perform! get::unique_name;

// Example usage:
let mut_a: Mut = ("name_a", 10, v: any => {
    println("mut_a changed to: " + display!(!v));
    if !v < 100 then v := !v + 1 else ()
}).#mutable;
let mut_b: Mut = ("name_b", 20).#mutable;
mut_a := !mut_a + 5;
mut_b := !mut_b * !mut_a;
println("mut_a: " + display!(!mut_a)); // 15
println("mut_b: " + display!(!mut_b)); // 300