let constraint Any::(Any: any) = import "lib/any.mu";
let constraint {
    List::(List: lambda) &
    map::(map: lambda) &
    allof::(allof: lambda)
} = import "lib/list.mu";

let constraint Pending: lambda = constraint (payload: any, continuation: lambda) => Pending::(payload, continuation);
let constraint Finished: lambda = constraint result: any => Finished::result;
let constraint Coroutine: any = [Pending::(Any, Any) | Finished::Any];

let constraint yield: lambda = constraint continutation: lambda => constraint value: any => Pending(value, continutation);
let constraint return: lambda = constraint _f: lambda => constraint value: any => Finished(value);

let constraint await: lambda = dyn_rec await:
    constraint continuation: lambda => 
    constraint coroutine: Coroutine => {
    match coroutine
        | constraint Pending::(payload: any, next_continuation: lambda) => {
            Pending::(payload, constraint v: any => await(continuation)(next_continuation(v)))
        }
        | constraint Finished::(value: any) => {
            continuation(value)
        }
        | panic
};

let constraint run_async: lambda = constraint statement: List(Coroutine) => {
    loop go: constraint statement: List(Coroutine) = statement;
    let constraint new_statements: any = statement.map(
        match
            | constraint Pending::(payload: any, continuation: lambda) => continuation(payload)
            | constraint Finished::(value: any) => Finished::value
            | panic
    );
    if new_statements.allof(
        match
            | assert Finished::Any => true
            | assert Pending::(Any, Any) => false
            | panic
    )
        then new_statements.map(constraint Finished::(value: any) => value)
        else go(new_statements)
};

// Example usage:
let constraint f: lambda = constraint (x: nat, id: nat) => {
    let constraint {
        nat_to_string::(nat_to_string: lambda) &
        println::(println: lambda)
    } = import "lib/string.mu";
    @yield ();
    discard println("In f: " + nat_to_string(x) + ", id: " + nat_to_string(id));
    @return x + 1;
};

let constraint g: lambda = constraint (x: nat, id: nat) => {
    let constraint y: nat = #await f(x, id);
    @yield ();
    @return x * y;
};


let constraint async_f: lambda = constraint (x: nat, id: nat) => {
    let constraint a: nat = #await f(x, id);
    let constraint b: nat = #await f(a, id) + #await g(x, id);
    @return b;
};

[async_f(10, 1), async_f(12, 2)].run_async