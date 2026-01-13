let Any::(Any: any) = import "lib/any.mu";
let {
    List::(List: lambda) &
    map::(map: lambda) &
    allof::(allof: lambda)
} = import "lib/list.mu";

let Pending: lambda = (payload: any, continuation: lambda) => Pending::(payload, continuation);
let Finished: lambda = result: any => Finished::result;
let Coroutine: any = [Pending::(Any, Any) | Finished::Any];

let yield: lambda = continutation: lambda => value: any => Pending(value, continutation);
let return: lambda = _f: lambda => value: any => Finished(value);

let await: lambda = dyn_rec await:
    continuation: lambda => 
    coroutine: Coroutine => {
    match coroutine
        | Pending::(payload: any, next_continuation: lambda) => {
            Pending::(payload, v: any => await(continuation)(next_continuation(v)))
        }
        | Finished::(value: any) => {
            continuation(value)
        }
        | panic
};

let run_async: lambda = statement: List(Coroutine) => {
    loop go: statement: List(Coroutine) = statement;
    let new_statements: any = statement.map(
        match
            | Pending::(payload: any, continuation: lambda) => continuation(payload)
            | Finished::(value: any) => Finished::value
            | panic
    );
    if new_statements.allof(
        match
            | Finished::Any => true
            | Pending::(Any, Any) => false
            | panic
    )
        then new_statements.map(Finished::(value: any) => value)
        else go(new_statements)
};

// Example usage:
let f: lambda = (x: nat, id: nat) => {
    let {
        nat_to_string::(nat_to_string: lambda) &
        println::(println: lambda)
    } = import "lib/string.mu";
    @yield ();
    discard println("In f: " + nat_to_string(x) + ", id: " + nat_to_string(id));
    @return x + 1;
};

let g: lambda = (x: nat, id: nat) => {
    let y: nat = #await f(x, id);
    @yield ();
    @return x * y;
};


let async_f: lambda = (x: nat, id: nat) => {
    let a: nat = #await f(x, id);
    let b: nat = #await f(a, id) + #await g(x, id);
    @return b;
};

[async_f(10, 1), async_f(12, 2)].run_async