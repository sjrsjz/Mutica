let constraint g: any = ();

let constraint f: any = rec f: match
    | constraint _T: _ => g f
    | panic;
f()

/*

上述代码会触发一个递归类型归约的bug，触发变量g未被捕获的错误。

具体表现为由于函数的表达式部分在reduce时被惰性处理（不进行归约），而fixpoint的reduce会导致未reduce的表达式会出现分叉行为，无法正确建立环形引用，最终导致变量捕获失败（捕获只对最外层有效，深度嵌套的环仍然指向旧环，无法正确捕获）

也就是说f()所调用的“f”是正确捕获的，但是g f中的“f”并没有正确捕获，导致最终运行时找不到变量。两个“f”实际上是不同的GC指针。

*/