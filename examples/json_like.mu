let list_pkg: any = import "lib/list.mu";
let string_pkg: any = import "lib/string.mu";
let List::(List: any) = list_pkg;
let iter::(iter: any) = list_pkg;
let String::(String: any) = string_pkg;
let println::(println: any) = string_pkg;
let print::(print: any) = string_pkg;


let Json: any = rec object: (
    List(object) |
    (rot [rec t: [() & (none @ t)]], object) |
    String |
    int |
    float |
    True::() |
    False::()
);

let print_json: any =  rec go: spacing: String => match
    | False::() => {
        discard print(spacing);
        println("false")
    }
    | True::() => {
        discard print(spacing);
        println("true")
    }
    | v: int => {
        discard print(spacing);
        println! v
    }
    | v: float => {
        discard print(spacing);
        println! v
    }
    | v: String => {
        discard print(spacing);
        println(v)
    }
    | (rot (k: [rec t: [() & (none @ t)]]), v: Json) => {
        discard print(spacing);
        discard println("> " + k);
        go(spacing + "  ")(v)
    }
    | vs: List(Json) => {
        iter(vs)(go(spacing))
    }
    | panic;

let my_json: Json = (
    (rot "name", "Mutica"),
    (rot "version",  "0.1.0"),
    (rot "features", (
        (rot "json-like", True::()),
        (rot "fixpoint", False::())
    )),
    (rot "dependencies", (
        "A", "B", "C"
    ))
);
print_json "" my_json