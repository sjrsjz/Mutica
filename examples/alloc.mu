let constraint string_pkg: any = import "lib/string.mu";
let constraint {
    String::(String: any) &
    println::(println: lambda)
} = string_pkg;

let constraint get: lambda = match | panic;
let constraint set: lambda = match | panic;

let constraint Pointer: any = (nat, nat);

extend get: constraint ClassA::(self: any) => constraint () => {
    let constraint data::(v: Pointer) = self;
    get!(v)
};

extend set: constraint ClassA::(self: any) => constraint value: String => {
    set!((let constraint data::(v: Pointer) = self; v), value)
};


let constraint classA: any = constraint v: String => constraint f: any => {
    let constraint obj: any = ClassA::{
        data::(alloc! v)
    };
    let constraint result: any = f(obj);
    discard dealloc!(
        let constraint ClassA::data::(v: Pointer) = obj;
        v
    );
    result    
};

classA "Hello, world!" {
    let constraint my_obj: any;
    discard println(my_obj.get());
    discard my_obj.set("Goodbye, world!");
    discard println(my_obj.get());
}