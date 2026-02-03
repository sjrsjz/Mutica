# Mutica 基本语法
Mutica 旨在提供最简洁且强大的语法结构，并在FP范式的基础上引入命令式风格的语法元素，以提升代码的可读性和易用性。

## 注释
Mutica 支持单行注释和多行注释两种形式：
- 单行注释以 `//` 开头，直到行尾结束。
- 多行注释以 `/*` 开头，以 `*/` 结尾，可以跨越多行。

## 括号

Mutica 认为括号**仅仅只是**优先级提升的手段，因此在设计之初就允许 `{}`、`[]`、`()` 三种括号互换使用，从而让代码更具表现力。例如：

```mutica
[1 + 2 * (3 + 4)] / {6 - 5}
```

所有使用括号的地方都可以互换，Mutica 不会因为括号的不同而改变表达式的含义。

注：与传统语言不同，花括号 `{}` 并不表示代码块，也没有特殊含义比如对象字面量或**作用域隔离**（这个涉及到自动CPS转换的实现细节，后续章节会介绍）。

## 元组

元组的**唯一**表示形式是使用逗号分隔的值列表，例如：

```mutica
1, 2, 3
```

单元素元组需要在元素后面添加逗号以示区分，例如：

```mutica
1,
```

元组可以嵌套，例如：

```mutica
(1, 2), [3, 4], {5, 6}
```

任何**不具备语法结构**的空白代码片段都会被求值为空元组（Unit），例如：

```mutica
    // 这是一个空白代码片段，表示空元组
```

大部分情况下由于优先级的原因，空元组需要使用括号包裹以避免歧义，例如：

```mutica
(), [], {}
```

## 基础字面量与标识符

### 数字与字符
Mutica 支持自然数与浮点数字面量：

```mutica
42
0xFF
0o77
0b1010
3.14
1e10
1.5e-3
```

浮点数要求小数点后至少一个数字，因此不会接受 `1.` 或 `.5` 这类形式，以避免与 `..` 冲突。

字符与字符串字面量：

```mutica
'a'
"hello"
```

字符串会被视为字符元组（语义上等价于 `('h', 'e', 'l', 'l', 'o')`）。

字符串与字符支持常见转义序列，例如 `\n`、`\r`、`\t`、`\\`、`\'`、`\"` 以及 `\u{...}`。

### 标识符与宏名
标识符由词法规则定义，普通标识符可直接使用；同时 Mutica 支持 `$"..."` 形式的字符串标识符，常用于宏或特殊命名：

```mutica
let $"my op": any = x: nat => x + 1;
```

在语法层面，`$"..."` 与普通标识符等价，但在着色上会被视为标识符或宏名。

此外，反引号包裹的名称会被识别为 attribute：

```mutica
`inline
```

## 类型与特殊类型

### 内置类型
Mutica 内置基础类型关键词：

- `nat`：自然数
- `float`：浮点数
- `char`：字符
- `true` / `false`：布尔字面量（语义上是 `op#true` / `op#false`）
- `any`：通配类型（语义和 `unknown`、`_` 相同）
- `never`：空并集（不可达）
- `unknown`：空交集（最宽）

### 通配符类型
`_` 作为通配符类型（解析为 `Wildcard`），用于忽略或泛化匹配：

```mutica
let _: any = 10;
```

## 命名空间
双冒号 `::` 用于命名空间构造：

```mutica
Math::add
```

语法形式为 `<tag> :: <expr>`，其中 `<tag>` 是标识符，`<expr>` 为任意表达式。

## 逻辑与运算符
Mutica 的运算符本质上是 `op#` 系列函数的语法糖：

- 算术：`+`、`-`（含一元负号）、`*`、`/`、`%`
- 比较：`==`、`!=`、`<`、`<=`、`>`、`>=`、`is`
- 逻辑：`&&`、`||`、`!`
- 赋值：`:=`（用于 `mut`）

例如：

```mutica
a + b        // op#add
a := b       // op#assign
a && b       // op#and
```

## 模式与泛型约束

Mutica 使用 `exist`、`assert`、`constraint` 三种泛型模式头部来表达约束：

```mutica
exist x: nat where { y: nat }
assert x: nat
constraint x: any
```

语义上分别对应：
- `exist <pattern> where <constraints>`：带约束的模式
- `assert <pattern>`：断言式模式
- `constraint <pattern>`：自动绑定模式

## 函数

### 函数本质
在 Mutica 中，**函数本质上就是对参数的模式匹配**。无论是简单的箭头函数还是复杂的 `match` 表达式，它们在底层都是统一的结构。

### 函数定义（Function Definition）
Mutica 使用箭头语法来定义最基础的函数：
```mutica
let f: any = (a: nat, b: nat) => a + b;
```
语法形式为 `<pattern> => <body>`，其中 `<pattern>` 是不可驳的参数模式，`<body>` 是函数体。

简单的箭头函数会被脱糖为 `match` 形式，例如上面的例子等价于：

```mutica
let f: any = match 
    | (a: nat, b: nat) => a + b 
    | panic
```

### Match 表达式（Function Implementation）
当需要处理多种情况时，使用 `match` 表达式。**`match` 定义了函数的多分支实现**。

`match` 支持两种形式：

1. **匿名匹配函数形式**（这是 `match` 的本体语义）：
```mutica
match
| 0 => "zero"
| n => "other"
| panic
```
这定义了一个函数，接受参数并根据分支进行匹配。

2. **立即调用形式**：
```mutica
match value
| 0 => "zero"
| n => "other"
| panic
```
这种形式是语法糖。`match <value> | <patterns...>` 会被脱糖为 `(match | <patterns...>)(<value>)`。也就是说，它是先定义了一个匿名匹配函数，然后立即应用于 `value`。

### Lambda 接口（Function Interface）
Mutica 区分了**实现**与**接口**。
- `match` 定义**怎么做**（实现）。
- `lambda` 定义**能接受什么**（接口）。

`lambda` 关键字用于声明一个函数类型的参数契约。它只包含模式，不包含执行体。

```mutica
// 定义一个类型，表示“接受 nat 或 string 的函数”
let MyFuncType: any = lambda 
    | x: nat 
    | s: String 
    | panic;
```

这一点非常重要：**`lambda` 不是函数值，它是对函数输入模式的描述**。在类型检查和多态分发中，`lambda` 扮演着接口的角色。

### 函数合并
函数对象可以通过 `__add!` VM原语进行合并，从而实现多态函数的定义，这体现了 Mutica 函数的结构化特征：

```mutica
let f: any = x: nat => x + 1;
let g: any = x: String => x + "!";
// 合并为 `match | x: nat => x + 1 | x: String => x + "!" | panic`
let h: any = __add!(f, g); 
```

### 函数调用
函数调用的语法形式为 `<function> <argument>`，其中 `<function>` 是要调用的函数表达式，`<argument>` 是传递给函数的参数表达式。由于模式匹配语义，Mutica 的函数实际上都是**单参数**函数，因此多参数函数调用需要使用元组传递参数，例如：

```mutica
f (x, y) // 不能写为 `f x, y`，因为它表示 `(f x), y`
```

同时 Mutica 支持 UFCS 风格的函数调用语法，任何 `<value>.<function>` 调用都会被脱糖为 `<function> <value>` 形式，例如：

```mutica
(x, y).f // 等效于 f (x, y)
```

Mutica 不会自动进行函数柯里化，用户只能通过编写高阶函数来实现柯里化效果，例如：

```mutica
let add: any = a: nat => b: nat => a + b;
let sum: nat = add 10 20;
```

同样的，函数调用也不会自动进行参数解包，用户需要显式地传递元组参数。例如：

```mutica
let multiply: any = (a: nat, b: nat) => a * b;
let product: nat = multiply (10, 20); // 正确
// let product_wrong: nat = multiply 10 20; // 错误，表示 (multiply 10) 20
```

由于模式匹配语义的存在，上述调用风格严格统一了命令式和柯里化风格的函数调用方式，使得代码更加一致和易读。

## 命令式风格语法

### let 绑定
Mutica 支持命令式风格的 `let` 绑定语法，例如：

```mutica
let x: nat = 10;
let y: nat = 20;
let sum: nat = x + y;
println! sum
```

`let` 语句被严格定义为 `let <pattern> = <value>; <expression>` 的形式，其中 `<pattern>` 是模式定义（默认为 `constraint`），`<value>` 是要绑定的值，`<expression>` 是后续表达式。

这种语法形式允许多行书写，并且每个 `let` 绑定后必须以分号 `;` 结尾，最后一个**表达式**不需要分号。

`let` 绑定最终会被脱糖为 `(<pattern> => <expression>)(<value>)` 的形式，因此可以在 `<value>` 中使用任意表达式。

注：Mutica 所有命令式风格的语句都强制要求以分号结尾（除了最后一个表达式），以避免歧义。

### let 声明
Mutica 支持命令式风格的 `let` 声明一个未被初始化的变量，例如：

```mutica
let constraint x: nat;
```

`let` 声明的语法形式为 `let <explicit-pattern>; <expression>`，其中 `<explicit-pattern>` 是**显式的**模式定义（必须指定 `exist`、`constraint`、`assert` 头部以进行区分），`<expression>` 是后续表达式。

`let` 声明的脱糖形式为 `<explicit-pattern> => <expression>`，与 `let` 绑定的区别在于没有立即调用。这使得它可以作为泛型模块的全局泛型参数使用。

### 自定义 let 绑定
Mutica 允许用户使用自定义绑定，例如：

```mutica
@my_let x: nat = 10;
```

自定义绑定的语法形式为 `@<binder> <pattern> = <value>; <expression>`，其中 `<binder>` 是上下文存在的函数绑定的名称，`<pattern>` 是模式定义，`<value>` 是要绑定的值，`<expression>` 是后续表达式。

自定义绑定最终会被脱糖为 `<binder>(<pattern> => <expression>)(<value>)` 的形式。这也意味着自定义绑定的 `<binder>` 必须是一个柯里化的高阶函数。

### 纯粹副作用语句
Mutica 提供纯粹副作用语句用于断言值为空元组（即 Unit）并不进行任何绑定，例如：

```mutica
println! "Hello, Mutica!";
```

语句的语法形式为 `<value>; <expression>`，其中 `<value>` 是要断言为空元组的值，`<expression>` 是后续表达式。

`<value>; <expression>` 语句等效为 `let () = <value>; <expression>`。

由于 Mutica 不允许直接丢弃有意义的表达式结果，因此纯粹副作用语句只能用于那些返回空元组的表达式，否则会导致运行时错误。

### if 表达式
Mutica 支持命令式风格的 `if` 表达式，例如：

```mutica
if x > 0 then
    println! "Positive"
else
    println! "Non-positive"
```

`if` 表达式的语法形式为 `if <condition> then <then_branch> else <else_branch>`，其中 `<condition>` 是条件表达式，`<then_branch>` 是条件为真时执行的表达式，`<else_branch>` 是条件为假时执行的表达式。

`if` 表达式最终会被脱糖为模式匹配的形式，因此可以在 `<condition>` 中使用任意返回值为 `true | false` 的表达式。

### for 表达式
Mutica 支持命令式风格的 `for` 表达式，例如：

```mutica
for i: nat = (1, 2, 3).iter in {
    println! i;
}
```

`for` 表达式的语法形式为 `for <pattern> = <func> in <expression>`，其中 `<pattern>` 是模式定义，`<func>` 是处理器函数，`<expression>` 是要处理的表达式。

Mutica 的 `for` 表达式**不是**迭代器循环，而是一个高阶函数调用的语法糖。它会被脱糖为 `<func>(<pattern> => <expression>)` 的形式。

Mutica 通过这种方式实现了命令式风格的循环（通过自定义迭代器函数），同时保持了函数式编程的纯粹性与极强的可组合性。

### 自定义 for 表达式
Mutica 允许用户使用自定义的 `for` 表达式，例如：

```mutica
@my_for i: nat = (1, 2, 3) in {
    println! i;
}
```

自定义 `for` 表达式的语法形式为 `@<forer> <pattern> = <func/value> in <expression>`，其中 `<forer>` 是上下文存在的高阶函数绑定的名称，`<pattern>` 是模式定义，`<func/value>` 是处理器函数或某个值，`<expression>` 是要处理的表达式。

自定义 `for` 表达式最终会被脱糖为 `<forer>(<func/value>)(<pattern> => <expression>)` 的形式。这也意味着自定义 `for` 表达式的 `<forer>` 必须是一个高阶函数。

### loop 循环
Mutica 支持 `loop` 语句用于创建一个带状态的递归循环，它提供了一种函数式的循环结构，例如：

```mutica
loop go: iter: nat = 10;
if iter > 0 then {
    println! iter;
    go(iter - 1)
} else {
    println! "Done"
}
```

`loop` 语句的语法形式为 `loop <label>: <pattern> = <value>; <expression>`，其中 `<label>` 是循环变量的名称（也是递归函数的名称），`<pattern>` 是循环参数模式，`<value>` 是初始值，`<expression>` 是循环体。

`loop` 语句最终会被脱糖为类似 `dyn_rec` 的使用 Y 组合子进行运行时自引用的结构：

```mutica
(dyn_rec <label>: <pattern> => <expression>) <value>
```

意味着在 `<expression>` 中，`<label>` 被绑定为一个函数，调用它即意味着进行下一次循环迭代，传递的新参数必须匹配 `<pattern>`。

### extend 扩展
Mutica 提供 `extend` 语句用于对当前作用域内的变量进行**增量**更新（通常用于字符串拼接或数字累加，取决于 `op#add` 的实现），这在命令式编程中非常常见。

```mutica
let s: String = "Hello";
extend s: ", world!";
println! s // 输出 "Hello, world!"
```

`extend` 语句的语法形式为 `extend <variable>: <value>; <expression>`。

它会被脱糖为 `let <variable> = <variable> + <value>; <expression>` 的形式。注意这里生成了新的变量绑定，遮盖了旧的同名变量。

### handle 语句
Mutica 提供 `handle` 语句用于处理计算过程中的**效应**（Effects）或复杂控制流，例如：

```mutica
handle result = 0 with my_handler;
// ... 这里的代码会被 my_handler 处理 ...
```

`handle` 语句有两种形式：
+ `handle <pattern> = <value> with <handler>; <expression>`：带初始值的 handle 语句。
+ `handle with <handler>; <expression>`：不带初始值的 handle 语句（初始值为 `discard` 模式）。

`handle` 语句将 `<expression>` 也就是后续的代码块包裹在 `<handler>` 提供的作用域中执行。这通常用于实现代数效应（Algebraic Effects）。


## 递归定义

Mutica 区分静态递归和动态递归：

### rec 静态递归（数据定义）
`rec` 关键字用于定义静态递归关系，**仅用于声明数据结构内部的递归**（如递归类型），不能用于函数自引用。

如果尝试使用静态递归进行函数值的自引用（即在函数体通过名字调用自己），会导致编译器拒绝编译，因为这涉及到跨越定义域的引用。

例如，通过 `rec` 定义递归的数据结构（如树）：

```mutica
let Tree: any = T: any => rec tree: (
    Empty::() | 
    Leaf::T | 
    Node::(tree, tree, T)
);
```

### dyn_rec 动态递归（函数定义）
函数自引用（递归调用）**必须**使用 `dyn_rec` 关键字。它是通过 Y 组合子（Fixpoint Combinator）在运行时实现“打结”来完成递归的。

```mutica
let fact: any = dyn_rec f: match 
    | 0 => 1 
    | n => n * f (n - 1) 
    | panic;
```

## 延迟与惰性求值

Mutica 提供原生语法支持延迟计算和惰性计算。

- **delay**: `delay <expr>` 语法将表达式 `<expr>` 封装为一个无参函数 `() => <expr>`。这通常用于推迟副作用的执行。
- **lazy**: `lazy <expr>` 语法创建一个惰性求值的值。`lazy` 会通过包装尝试阻断VM对 `invoke` 类型的 beta 归约。

## mut 与赋值

`mut` 关键字用于标记可变值：

```mutica
let x: mut nat = mut 10;
x := 20;
```

`:=` 是可变赋值的语法糖，对应 `op#assign`。

## invoke 语法

Mutica 支持带 continuation 的调用语法：

```mutica
f x |> k
```

其语义等价于对 `f` 进行 CPS 调用，并将 `k` 作为 continuation 传入。