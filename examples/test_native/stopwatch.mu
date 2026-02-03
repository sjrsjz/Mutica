let stopwatch: any = stopwatch!();
{
    loop go: i: nat = 0;
    if i < 500000 then go(i + 1) else ()
};
stopwatch(), stopwatch