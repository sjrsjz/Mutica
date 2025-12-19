# Mutica

<div align="center">

**An experimental, statically-typed functional programming language with advanced coinductive type system.**

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE) 
[![Rust](https://img.shields.io/badge/rust-1.80%2B-orange.svg)](https://www.rust-lang.org/)

</div>

## 📖 Overview

Mutica is an experimental, statically-typed functional programming language featuring an advanced **coinductive type system** for precise structural type checking. The language supports powerful pattern matching, effect handlers, and a rule-based constraint validation system that goes far beyond traditional subtyping.

### Key Features

- 🎯 **Coinductive Type System**: Natively supports recursive types and validation of potentially infinite data structures through coinductive reasoning.
- 🔀 **Rule-Based Constraint Validation**: A powerful (`is`) operator for checking type compatibility, governed by a purely syntactic set of rules beyond traditional subtyping.
- 🎭 **Advanced Pattern Matching**: Sophisticated destructuring with exhaustive pattern matching and type-safe guards.
- 📦 **Label-based Namespaces**: Type isolation through labels, enabling algebraic data types like `Maybe` and `Either`.
- 💫 **Effect Handlers**: Built-in support for algebraic effects through `perform!` and `handle...with` constructs.
- 🛡️ **Meta-level Type Operators**: Unique operators like `eq`, `rot`, and `subof` that manipulate constraint validation at the type level.
- 📚 **Module System**: Import-based modular code organization with `import` statements.
- ♻️ **Automatic Garbage Collection**: Employs `arc-gc` with cycle detection for efficient memory management.

## 🚀 Quick Start

### Installation

Ensure you have Rust 1.80+ installed. Then, clone and build the project:

```bash
git clone https://github.com/sjrsjz/Mutica.git
cd Mutica
cargo build --release
```

### Run Examples

```bash
# Run a single file using cargo
cargo run -- run examples/fib.mu

# Or use the compiled executable directly
./target/release/mutica run examples/fib.mu

# Check version
cargo run -- version
```

## 📚 Syntax Overview

### Basic Types

```mutica
// Integer
let x: int = 42;

// Character
let c: char = 'A';

// Tuple
let pair: (int, int) = (1, 2);

// AnyOf Type
let value: (int | char) = 42;

// AllOf Type (used for records/structs)
let point: { x::int & y::int } = { x::1 & y::2 };

// Top Type (any)
let anything: any = 42; // `any` is the supertype of all conventional types
let _ = 42;             // An underscore can be used directly to assert a type constraint
```

### Function Definitions

```mutica
// A function that accepts an integer
let add_one: any = (x: int) => x + 1; // `=>` defines a function

// A recursive function using `rec` with pattern matching
let fib: any = rec f: match
    | eq 0 => 0
    | eq 1 => 1
    | n: int => f(n - 1) + f(n - 2)
    | panic; // Asserts that the match is exhaustive for the input `n: int`
```

### Constraint Checks (`is`)

The `is` operator is not traditional subtyping, but a check to see if a type fulfills the constraints of another.

```mutica
// A value fulfills the constraint of its general type
1 is int                           // true

// A more specific record fulfills the constraint of a more general one
{ x::1 & y::2 } is { x::int }      // true
```

### Namespaces and ADTs

```mutica
// Define labeled constructors
let Just: any = T: any => Just::T;
let Nothing: any = Nothing::();

// Define the Maybe type using a union
let Maybe: any = T: any => (Just T | Nothing);

// Use pattern matching on labeled types
let map: any = v: Maybe(any) => f: any => 
    match v
        | Just::(x: any) => Just(f(x))
        | Nothing::() => Nothing
        | panic;
```

### Struct-like Representation

```mutica
// Use intersection types to simulate a struct
let Point: any = (x: int, y: int, z: int) => { x::x & y::y & z::z };

let p: any = Point(1, 2, 3);

// Deconstructuring
let { x::(x: int) & z::(z: int) } = p;
```

### Effect Handlers

```mutica
// Define effect handlers
let handler: any = match
    | GetA::() => 42
    | GetB::() => 84
    | panic;

// Use effects with handlers
handle z: int = 1 with handler;
let x: int = perform! GetA::();
let y: int = perform! GetB::();
x, y, z  // Results: 42, 84, 1
```

### Module System

```mutica
// Import from another file
let pkg: any = import "lib/maybe.mu";

// Destructure imported values
let {
    Just::(Just: any) &
    Nothing::(Nothing: any) &
    map::(map: any)
} = pkg;

// Use imported functions
let v1: any = Just(41);
map(v1)(x: int => x + 1)  // Results: Just(42)
```

### UFCS

```mutica
let {
    Just::(Just: any) &
    map::(map: any)
} = import "lib/maybe.mu";
// Using UFCS to call functions as methods
let v1: any = Just(41);
v1.map(x: int => x + 1)  // Results: Just(42)
```

## 🎯 Example Programs

### Fibonacci

```mutica
let fib: any = rec f: match
    | eq 0 => 0
    | eq 1 => 1
    | n: int => f(n - 1) + f(n - 2)
    | panic;

fib(10) // Computes the 10th Fibonacci number
```

### IO Example

```mutica
let List: any = (T: any) => rec list: (() | T @ list);
let print_chars: any = rec print_chars: str: List(char) =>
    match str
        | () => ()
        | (head: char) @ (tail: any) => (discard print!(head); print_chars(tail))
        | panic;

print_chars("Hello, world!\n")
```

## 🏗️ Architecture

The Mutica implementation is organized into multiple crates:

- **`mutica-compiler`**: Parsing and AST construction using `lalrpop` and `logos`
- **`mutica-core`**: Type system, scheduler, and runtime
- **`mutica-semantic`**: Semantic analysis and LSP support
- **`mutica`**: Main CLI binary

### Key Dependencies

- **`clap`**: Command-line argument parsing
- **`lalrpop`**: Parser generator for Mutica grammar
- **`logos`**: Fast lexical analysis
- **`ariadne`**: Beautiful diagnostic error reports
- **`arc-gc`**: Cycle-detecting garbage collector
- **`stacksafe`**: Stack overflow protection
- **`tokio`**: Async runtime for the scheduler

### Compilation Pipeline

1.  **Parsing**: Source code is parsed into an Abstract Syntax Tree (AST) using LALRPOP
2.  **Multi-file Building**: Import resolution and module system handling
3.  **Linearization**: AST is linearized to explicit control flow representation
4.  **Flow Analysis**: Variable definedness and usage validation with warnings/errors
5.  **Type Building**: AST is converted into coinductive `Type` representation
6.  **Reduction**: Types are reduced to normal form through the constraint system
7.  **Execution**: Linear scheduler evaluates the reduced type with effect handling

## 🤝 Contributing

Contributions are highly welcome! Please feel free to open an Issue to discuss ideas or submit a Pull Request with improvements.

## 📄 License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.
