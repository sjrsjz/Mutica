# Mutica

<div align="center">

**An experimental typed programming language based on Continuation-Passing Style (CPS)**

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)  
[![Rust](https://img.shields.io/badge/rust-1.85%2B-orange.svg)](https://www.rust-lang.org/)

</div>

## 📖 Overview

Mutica is an experimental, dynamically strong-typed functional programming language that uses Continuation-Passing Style (CPS) as its core computation model. It features an advanced coinductive type system supporting structural subtyping, pattern matching, recursive types, and more.

### Key Features

- 🔄 CPS transformation: all expressions are automatically converted to Continuation-Passing Style  
- 🎯 Coinductive type system: supports recursive types and complex type relationships  
- 🔀 Subtyping: flexible type compatibility checks (`<:`)  
- 🎭 Pattern matching: powerful destructuring and pattern matching capabilities  
- 🔁 Recursive types: native support for recursive functions and data structures  
- 📦 Namespaces: label-based namespaces for type isolation  
- ♻️ Automatic garbage collection: arc-gc based cycle detection  
- 🛡️ Enforced variable usage: strict checks to avoid unused variables

## 🚀 Quick Start

### Installation

Make sure you have the Rust toolchain (1.85+) installed, then clone and build the project:

```bash
git clone https://github.com/yourusername/Mutica.git
cd Mutica
cargo build --release
```

### Run examples

```bash
# Run a single file
cargo run -- run examples/fib.mu

# Or use the compiled executable
./target/release/mutica run examples/hello.mu
```

## 📚 Syntax Overview

### Basic types

```mutica
// Integer
let x: int = 42;

// Character
let c: char = 'A';

// Tuple
let pair: (int, int) = (1, 2);

// Union
let value: (int | char) = 42;

// Specialized type (note it's not an intersection of values, 1 & 2 != false)
let labeled: { x::int & y::int } = { x::1 & y::2 };
```

### Function definitions

```mutica
// Simple (partial) function
let add_one: any = (x: int) |-> x + 1; // `|->` means input type must be a subtype described by the parameter pattern

// Recursive function
let fib: any = rec f: (n: int) |-> 
    match n
        | 0 => 0
        | 1 => 1
        | ! => f(n - 1) + f(n - 2); // `!` is the default branch

// Total function
let safe_div: any = (x: int, y: int) -> x / y \ "Invalid Input Type"; // `\` denotes a branch when pattern match fails
```

### Pattern matching

```mutica
// Destructure a tuple
let (x: int, y: int) = (1, 2);

// Match expression
match value
    | 0 => "zero"
    | (x: int, y: int) => "pair"
    | panic         // assert that input pattern is fully covered by match branches
```

### List operations

```mutica
// List literal
let lst: any = @(1, 2, 3, 4, 5);

// Recursive list type definition
let int_list: any = rec list: (() | (int, list));

// List processing
let append: any = rec append: (list1: any, list2: any) |->
    match list1
        | () => list2
        | (head: int, tail: any) => (head, append(tail, list2))
        | panic;
```

### Namespaces (labels)

```mutica
// Define labeled constructors
let Just: any = T: any |-> Just::T;
let Nothing: any = Nothing::();

// Maybe type
let Maybe: any = T: any |-> (Just T | Nothing);

// Pattern match with labels
match value
    | Just::(x: int) => x + 1
    | Nothing::() => 0
    | panic;
```

### Struct-like representation

```mutica
// Use intersection types to simulate a struct
let Point: any = (x: int, y: int) |-> { x::x & y::y };

let p: any = Point(3, 4);
let x_coord: any = p.x;  // field access
let y_coord: any = p.y;
```

### Subtyping checks

```mutica
// Type compatibility checks
1 <: int                           // true
(1, 2) <: (int, int)               // true
{ x::1 & y::2 } <: { x::int }      // true (width subtyping)
```

## 🎯 Example Programs

### Fibonacci

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

### Iterator pattern

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

iter(sum)(0, list)  // result: 15
```

## 🏗️ Type System Design

### Core types

- **TypeBound**: type bounds (⊤ and ⊥)  
- **Integer / IntegerValue**: integer type and integer value type  
- **Character / CharacterValue**: character type and character value type  
- **Tuple**: tuple type  
- **List**: list type (optimized representation with nested tuples)  
- **Closure**: closure type, supports pattern-matching parameters  
- **Generalize / Specialize**: generalization and specialization  
- **FixPoint**: fixed-point type (recursive types)  
- **Invoke**: CPS-type application  
- **Variable**: type variables  
- **Namespace**: namespace / label type  
- **Pattern**: pattern types  
- **Opcode**: built-in operation functions

### CPS transformation

All Mutica computations are performed via CPS transformation. For example:

```
Expression: f(x)
CPS form: CPS(f, x, λresult. continuation)
```

This makes control flow explicit, facilitating advanced features like error handling and coroutines.

### Type checking

The type system uses coinduction, enabling:

- natural representation of recursive types  
- handling cyclic references  
- covariant and contravariant subtyping relations  
- exhaustiveness checks for pattern matching

## 🛠️ Tech Stack

- Diagnostic: ariadne
- Parser: lalrpop
- Lexer: logos  
- Garbage collection: rust-arc-gc  
- Stack safety: stacksafe

## 🎓 Design Principles

Mutica's design is inspired by:

1. Continuation-Passing Style: making control flow explicit  
2. Coinductive types: handling infinite and recursive structures  
3. Subtype polymorphism: flexible type compatibility  
4. Algebraic data types: implemented via unions and intersections

## 🤝 Contributing

Contributions welcome! Please open an Issue or submit a Pull Request.

## 📄 License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file.

## 🔗 Resources

- [Continuation-Passing Style](https://en.wikipedia.org/wiki/Continuation-passing_style)  
- [Coinductive Types](https://en.wikipedia.org/wiki/Coinduction)  
- [Substructural Type System](https://en.wikipedia.org/wiki/Substructural_type_system)
