let string_pkg: any = import "lib/string.mu";
let String: any = string_pkg.String;
let classA: any = v: String -> f: any -> {
    let obj: any = rec self: {
        data::(alloc! v) &
        get::(() -> get!(self.data)) &
        set::((value: String) -> set!(self.data, value))
    };
    let result: any = f(obj);
    discard dealloc!(obj.data);
    result    
};

classA "Hello, world!" {
    let my_obj: any;
    discard string_pkg.println(my_obj.get());
    discard my_obj.set("Goodbye, world!");
    discard string_pkg.println(my_obj.get());
}