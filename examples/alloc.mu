let string_pkg: any = import "lib/string.mu";
let {
    String::(String: any) &
    println::(println: any)
} = string_pkg;

let get: any = match | panic;
let set: any = match | panic;

extend get: ClassA::(self: any) => () => {
    let data::(v: any) = self;
    get!(v)
};

extend set: ClassA::(self: any) => value: String => {
    set!((let data::(v: any) = self; v), value)
};


let classA: any = v: String => f: any => {
    let obj: any = ClassA::{
        data::(alloc! v)
    };
    let result: any = f(obj);
    discard dealloc!(
        let ClassA::data::(v: any) = obj;
        v
    );
    result    
};

classA "Hello, world!" {
    let my_obj: any;
    discard println(my_obj.get());
    discard my_obj.set("Goodbye, world!");
    discard println(my_obj.get());
}