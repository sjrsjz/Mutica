let throw_panic: any = match
    | v: any => {
        let never = Panic::v;
    }
    | panic;

throw_panic::throw_panic