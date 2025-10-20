let result: int = handle {
    let x: int = perform! GetA::();
    let y: int = handle {
        let a: int = perform! GetC::();
        let b: int = perform! GetD::(a + 10);
        b
    } with 
        | GetC::() => 100
        | GetD::(x: int) => x * 3
        | panic;
    let z: int = perform! GetB::(x + y);
    z
} with
    | GetA::() => 42
    | GetB::(x: int) => x / 2
    | panic;
result