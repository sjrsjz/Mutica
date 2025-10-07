# Mutica

<div align="center">

**一个基于 Continuation-Passing Style 的实验性类型化编程语言**

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/rust-1.85%2B-orange.svg)](https://www.rust-lang.org/)

</div>

## 📖 简介

Mutica 是一个实验性的动态强类型函数式编程语言，采用 **Continuation-Passing Style (CPS)** 作为其核心计算模型。它具有一个先进的余归纳类型系统（Coinductive Type System），支持结构化子类型、模式匹配、递归类型等特性。

### 核心特性

- 🔄 **CPS 转换**：所有表达式自动转换为 Continuation-Passing Style
- 🎯 **余归纳类型系统**：支持递归类型和复杂的类型关系
- 🔀 **子类型关系**：灵活的类型兼容性判断 (`<:`)
- 🎭 **模式匹配**：强大的模式匹配和解构能力
- 🔁 **递归类型**：原生支持递归函数和递归数据结构
- 📦 **命名空间**：通过标签命名空间实现类型隔离
- ♻️ **自动垃圾回收**：基于 arc-gc 的循环引用检测

## 🚀 快速开始

### 安装

确保您已安装 Rust 工具链（1.85+），然后克隆并构建项目：

```bash
git clone https://github.com/yourusername/Mutica.git
cd Mutica
cargo build --release
```

### 运行示例

```bash
# 运行单个文件
cargo run -- run examples/fib.mu

# 或使用编译后的可执行文件
./target/release/mutica run examples/hello.mu
```

## 📚 语法概览

### 基本类型

```mutica
// 整数类型
let x: int = 42;

// 字符类型
let c: char = 'A';

// 元组类型
let pair: (int, int) = (1, 2);

// 联合类型
let value: (int | char) = 42;

// 特化类型 (注意它不是交集类型，1 & 2 != false)
let labeled: { x::int & y::int } = { x::1 & y::2 };
```

### 函数定义

```mutica
// 简单函数 (部分函数)
let add_one: any = (x: int) |-> x + 1; // `|->` 表示断言输入类型必须为函数参数模式所描述的类型的子类型

// 递归函数
let fib: any = rec f: (n: int) |->
    match n
        | 0 => 0
        | 1 => 1
        | ! => f(n - 1) + f(n - 2); // `!` 表示默认分支

// 全函数
let safe_div: any = (x: int, y: int) -> x / y \ "Invalid Input Type"; // `\` 表示模式匹配失败分支
```

### 模式匹配

```mutica
// 解构元组
let (x: int, y: int) = (1, 2);

// Match 表达式
match value
    | 0 => "zero"
    | (x: int, y: int) => "pair"
    | panic         // 断言输入模式完全匹配 match 分支
```

### 列表操作

```mutica
// 列表字面量
let lst: any = @(1, 2, 3, 4, 5);

// 递归列表类型定义
let int_list: any = rec list: (() | (int, list));

// 列表处理
let append: any = rec append: (list1: any, list2: any) |->
    match list1
        | () => list2
        | (head: int, tail: any) => (head, append(tail, list2))
        | panic;
```

### 命名空间（标签）

```mutica
// 定义带标签的类型
let Just: any = T: any |-> Just::T;
let Nothing: any = Nothing::();

// Maybe 类型
let Maybe: any = T: any |-> (Just T | Nothing);

// 模式匹配标签
match value
    | Just::(x: int) => x + 1
    | Nothing::() => 0
    | panic;
```

### 结构体模拟

```mutica
// 使用交集类型实现结构体
let Point: any = (x: int, y: int) |-> { x::x & y::y };

let p: any = Point(3, 4);
let x_coord: any = p.x;  // 访问字段
let y_coord: any = p.y;
```

### 子类型检查

```mutica
// 类型兼容性检查
1 <: int                           // true
(1, 2) <: (int, int)               // true
{ x::1 & y::2 } <: { x::int }      // true (宽度子类型)
```

## 🎯 示例程序

### Fibonacci 数列

```mutica
let fib: any = rec f: (n: int) |-> 
    match n
        | 0 => 0
        | 1 => 1
        | ! => f(n - 1) + f(n - 2);
fib(10)
```

### Hello World

```mutica
let print_chars: any = rec print_chars: (chars: (() | (char, any))) |->
    match chars
        | () => ()
        | (head: char, tail: any) => (discard print(head); print_chars(tail))
        | panic;
print_chars("Hello, world!\n")
```

### 迭代器模式

```mutica
let list: any = @(1, 2, 3, 4, 5);
let break: any = v: any |-> Break::v;
let continue: any = v: any |-> Continue::v;

let iter: any = f: any |-> rec iter: (state: any) |-> 
    match f(state)
        | Continue::(next_state: any) => iter(next_state)
        | Break::(result: any) => result
        | panic;

let sum: any = (count: int, lst: (() | (int, any))) |-> 
    match lst
        | () => break count
        | (head: int, tail: any) => continue(count + head, tail)
        | panic;

iter(sum)(0, list)  // 结果: 15
```

## 🏗️ 类型系统设计

### 核心类型

- **TypeBound**: 类型边界（⊤ 和 ⊥）
- **Integer** / **IntegerValue**: 整数类型和整数值类型
- **Character** / **CharacterValue**: 字符类型和字符值类型
- **Tuple**: 元组类型
- **List**: 列表类型（嵌套元组的优化表示）
- **Closure**: 闭包类型，支持模式匹配参数
- **Generalize** / **Specialize**: 泛化和特化
- **FixPoint**: 不动点类型（递归类型）
- **Invoke**: CPS 类型应用
- **Variable**: 类型变量
- **Namespace**: 命名空间/标签类型
- **Pattern**: 模式类型
- **Opcode**: 内置操作函数

### CPS 转换

Mutica 的所有计算都通过 CPS 转换进行。例如：

```
表达式: f(x)
CPS形式: CPS(f, x, λresult. continuation)
```

这使得程序的控制流显式化，便于实现高级特性如异常处理和协程。

### 类型检查

类型系统采用**余归纳**（Coinductive）方法，支持：

- 递归类型的自然表示
- 循环引用的处理
- 子类型关系的协变和逆变
- 模式匹配的完备性检查

## 📦 项目结构

```
Mutica/
├── src/
│   ├── main.rs              # CLI 入口
│   ├── lib.rs               # 核心库入口
│   ├── parser/              # 解析器
│   │   ├── grammar.lalrpop  # LALRPOP 语法定义
│   │   ├── lexer.rs         # 词法分析器
│   │   └── ast.rs           # 抽象语法树
│   ├── types/               # 类型系统
│   │   ├── mod.rs           # 类型模块入口
│   │   ├── closure.rs       # 闭包类型
│   │   ├── invoke.rs        # CPS 调用
│   │   └── ...              # 其他类型
│   ├── scheduler/           # 执行调度器
│   └── util/                # 工具模块
├── examples/                # 示例程序
│   ├── fib.mu
│   ├── io.mu
│   ├── struct.mu
│   └── ...
└── Cargo.toml
```

## 🛠️ 技术栈

- **语言实现**: Rust
- **解析器**: LALRPOP (LR parser generator)
- **词法分析**: Logos
- **垃圾回收**: rust-arc-gc (Arc-based GC with cycle detection)
- **栈安全**: stacksafe (Stack overflow prevention)

## 🎓 设计理念

Mutica 的设计受到以下理论的启发：

1. **Continuation-Passing Style**: 将控制流显式化
2. **余归纳类型理论**: 处理无限和递归结构
3. **子类型多态**: 灵活的类型兼容性
4. **代数数据类型**: 通过联合和交集类型实现

## 🤝 贡献

欢迎贡献！请随时提交 Issue 或 Pull Request。

## 📄 许可证

本项目采用 MIT 许可证 - 详见 [LICENSE](LICENSE) 文件。

## 🔗 相关资源

- [Continuation-Passing Style](https://en.wikipedia.org/wiki/Continuation-passing_style)
- [Coinductive Types](https://en.wikipedia.org/wiki/Coinduction)
- [Substructural Type System](https://en.wikipedia.org/wiki/Substructural_type_system)
