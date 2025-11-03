# Mutica

<div align="center">

**An experimental, statically-typed programming language based on a pure Continuation-Passing Style (CPS) core.**

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE) 
[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org/)

</div>

## 📖 Overview

Mutica is an experimental, statically-typed functional programming language that uses Continuation-Passing Style (CPS) as its core computation model. Its primary innovation is an advanced **constraint validation system** built upon a coinductive core, enabling precise, structural type checks far beyond traditional subtyping.

### Key Features

- 🔄 **Continuation-Passing Style**: All expressions are automatically converted to CPS, making control flow explicit and powerful.
- 🎯 **Coinductive Core**: Natively supports recursive types and the validation of potentially infinite data structures.
- 🔀 **Rule-Based Constraint System**: A powerful (`<:`) operator for checking type compatibility and validating complex invariants, governed by a purely syntactic set of rules.
- 🎭 **Advanced Pattern Matching**: Sophisticated destructuring and exhaustive pattern matching capabilities.
- 📦 **Label-based Namespaces**: Provides type isolation and allows for the creation of algebraic data types like `Maybe`.
- 🛡️ **Meta-level Type Modifiers**: Unique operators like `neg` and `rot` that manipulate the constraint validation process itself, enabling powerful type-level metaprogramming.
- ♻️ **Automatic Garbage Collection**: Employs `arc-gc` for efficient cycle detection and memory management.

## 🚀 Quick Start

### Installation

Ensure you have the Rust toolchain installed. Then, clone and build the project:

```bash
git clone https://github.com/yourusername/Mutica.git
cd Mutica
cargo build --release
```

### Run Examples

```bash
# Run a single file using cargo
cargo run -- run examples/fib.mu

# Or use the compiled executable directly
./target/release/mutica run examples/fib.mu
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

// Generalize Type
let value: (int | char) = 42;

// Specialize Type (used for records/structs)
let point: { x::int & y::int } = { x::1 & y::2 };

// Top Type (any)
let anything: any = 42; // `any` is the supertype of all conventional types
let _ = 42;             // An underscore can be used directly to assert a type constraint
```

### Function Definitions

```mutica
// A function that accepts an integer
let add_one: any = (x: int) => x + 1; // `=>` defines a function

// A recursive function using `rec`
let fib: any = rec f: (n: int) => 
    match n
        | eq 0 => 0
        | eq 1 => 1
        | _ => f(n - 1) + f(n - 2)
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
match some_maybe_value
    | Just::(x: int) => x + 1
    | Nothing::() => 0
    | panic;
```

### Struct-like Representation

```mutica
// Use intersection types to simulate a struct
let Point: any = (x_val: int, y_val: int) => { x::x_val & y::y_val };

let p: any = Point(3, 4);

// Deconstructuring
let x::(x_value: int) = p;
let y::(y_value: int) = p;
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

## 🏗️ Compiler

The Mutica compiler is written in Rust and uses the following libraries:

- **`clap`**: for command-line argument parsing.
- **`lalrpop`**: for parsing the Mutica grammar.
- **`logos`**: for lexical analysis.
- **`ariadne`**: for generating beautiful error reports.
- **`arc-gc`**: for garbage collection.
- **`stacksafe`**: for stack safety.

The compilation process consists of the following stages:

1.  **Parsing**: The source code is parsed into an Abstract Syntax Tree (AST).
2.  **Linearization**: The AST is linearized to make control flow explicit.
3.  **Flow Analysis**: The linearized AST is analyzed to ensure that all variables are defined before they are used.
4.  **Type Building**: The AST is converted into a `Type` representation.
5.  **Reduction**: The `Type` is reduced to its normal form.

## 🤝 Contributing

Contributions are highly welcome! Please feel free to open an Issue to discuss ideas or submit a Pull Request with improvements.

## 📄 License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.
