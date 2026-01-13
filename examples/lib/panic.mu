let throw_panic: lambda = match
    | v: any => {
        let never = Panic::v;
    }
    | panic;

throw_panic::throw_panic