let string_pkg: any = import "lib/string.mu";
let {
    String::(String: any) &
    println::(println: [lambda | panic])
} = string_pkg;

let get: (lambda | panic) = match | panic;
let set: (lambda | panic) = match | panic;

extend get: ClassA::(self: any) => () => {
    let data::(mut v: any) = self;
    v
};

extend set: ClassA::(self: any) => value: String => {
    let data::(v: any) = self;
    discard v := value;
};


let classA: any = v: String => f: any => {
    let obj: any = ClassA::{
        data::(mut v)
    };
    let result: any = f(obj);
    result
};

classA "Hello, world!" {
    let constraint my_obj: any;
    discard println(my_obj.get());
    discard my_obj.set("Goodbye, world!");
    discard println(my_obj.get());
}