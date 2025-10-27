let list_pkg: any = import "lib/list.mu";
let string_pkg: any = import "lib/string.mu";
let List: any = list_pkg.List;
let String: any = string_pkg.String;

let Json: any = rec object: (
    List(object) |
    (rot [rec t: [() & (none @ t)]], object) |
    String |
    int |
    float |
    True::() |
    False::()
);

let print_json: any =  rec go: spacing: String -> match
    | False::() => {
        discard string_pkg.print(spacing);
        string_pkg.println("false")
    }
    | True::() => {
        discard string_pkg.print(spacing);
        string_pkg.println("true")
    }
    | v: int => {
        discard string_pkg.print(spacing);
        println! v
    }
    | v: float => {
        discard string_pkg.print(spacing);
        println! v
    }
    | v: String => {
        discard string_pkg.print(spacing);
        string_pkg.println(v)
    }
    | (rot (k: [rec t: [() & (none @ t)]]), v: Json) => {
        discard string_pkg.print(spacing);
        discard string_pkg.println("> " + k);
        go(spacing + "  ")(v)
    }
    | vs: List(Json) => {
        list_pkg.iter(vs)(go(spacing))
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