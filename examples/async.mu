let {
    List::(List: any) &
    map::(map: any) &
    allof::(allof: any)
} = import "lib/list.mu";

let Pending: any = (payload: any, continuation: any) => Pending::(payload, continuation);
let Finished: any = result: any => Finished::result;
let Coroutine: any = [Pending::(any, any) | Finished::any];

let yield: any = continutation: any => value: any => Pending(value, continutation);
let return: any = _f: any => value: any => Finished(value);

let await: any = dyn_rec await:
    continuation: any => 
    coroutine: Coroutine => {
    match coroutine
        | Pending::(payload: any, next_continuation: any) => {
            Pending::(payload, v: any => await(continuation)(next_continuation(v)))
        }
        | Finished::(value: any) => {
            continuation(value)
        }
        | panic
};

let run_async: any = statement: List(Coroutine) => {
    loop go: statement: List(Coroutine) = statement;
    let new_statements: any = statement.map(
        c: Coroutine => match c
            | Pending::(payload: any, continuation: any) => continuation(payload)
            | Finished::(value: any) => Finished::value
            | panic
    );
    if new_statements.allof(
         c: Coroutine => match c
            | Finished::any => true
            | Pending::(any, any) => false
            | panic
    )
        then new_statements.map(c: Coroutine => match c
            | Finished::(value: any) => value
            | panic
        )
        else go(new_statements)
};

// Example usage:
let f: any = (x: nat, id: nat) => {
    let {
        nat_to_string::(nat_to_string: any) &
        println::(println: any)
    } = import "lib/string.mu";
    @yield ();
    discard println("In f: " + nat_to_string(x) + ", id: " + nat_to_string(id));
    @return x + 1;
};

let g: any = (x: nat, id: nat) => {
    let y: nat = #await f(x, id);
    @yield ();
    @return x * y;
};


let async_f: any = (x: nat, id: nat) => {
    let a: nat = #await f(x, id);
    let b: nat = #await f(a, id) + #await g(x, id);
    @return b;
};

[async_f(10, 1), async_f(12, 2)].run_async