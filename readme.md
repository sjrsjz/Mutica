# Mutica

<div align="center">

**An experimental, statically-typed programming language based on a pure Continuation-Passing Style (CPS) core.**

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/rust-1.85%2B-orange.svg)](https://www.rust-lang.org/)

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

Ensure you have the Rust toolchain (version 1.85 or newer) installed. Then, clone and build the project:

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
./target/release/mutica run examples/hello.mu
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
let add_one: any = (x: int) |-> x + 1; // `|->` defines a function

// A recursive function using `rec`
let fib: any = rec f: (n: int) |-> 
    match n
        | 0 => 0
        | 1 => 1
        | _ => f(n - 1) + f(n - 2)
        | panic; // Asserts that the match is exhaustive for the input `n: int`
```

### Constraint Checks (`<:`)

The `<:` operator is not traditional subtyping, but a check to see if a type fulfills the constraints of another.

```mutica
// A value fulfills the constraint of its general type
1 <: int                           // true

// A more specific record fulfills the constraint of a more general one
{ x::1 & y::2 } <: { x::int }      // true
```

### Namespaces and ADTs

```mutica
// Define labeled constructors
let Just: any = T: any |-> Just::T;
let Nothing: any = Nothing::();

// Define the Maybe type using a union
let Maybe: any = T: any |-> (Just T | Nothing);

// Use pattern matching on labeled types
match some_maybe_value
    | Just::(x: int) => x + 1
    | Nothing::() => 0
    | panic;
```

### Struct-like Representation

```mutica
// Use intersection types to simulate a struct
let Point: any = (x_val: int, y_val: int) |-> { x::x_val & y::y_val };

let p: any = Point(3, 4);

// Access fields using labels
let x_coord: any = p.x;
let y_coord: any = p.y;
```

## 🎯 Example Programs

### Fibonacci

```mutica
let fib: any = rec f: (n: int) |-> 
    match n
        | 0 => 0
        | 1 => 1
        | _ => f(n - 1) + f(n - 2)
        | panic;

fib(10) // Computes the 10th Fibonacci number
```

### Hello World

```mutica
// A recursive function to print a list of characters
let print_chars: any = rec print_chars: (chars: (() | (char, any))) |->
    match chars
        | () => ()
        | (head: char, tail: any) => (discard print(head); print_chars(tail))
        | panic;

print_chars("Hello, world!\n")
```

## 🏗️ The Constraint System

Mutica's core innovation is its **rule-based constraint validation system**. Instead of being limited by traditional set-theoretic semantics, the `<:` operator acts as a programmable predicate whose behavior is defined by a consistent set of syntactic rewrite rules.

This coinductively-defined system enables:

- **Precise Structural Validation**: Accurately models traditional subtyping behaviors like width and depth for records, and covariance/contravariance for functions, as a subset of its capabilities.

- **Meta-level Type Modifiers**: Introduces unique type modifiers like `neg` and `rot` that operate on the validation process itself, allowing for powerful meta-programming at the type level.
    - **`neg` (Negation)**: Reverses the boolean outcome of a constraint check, enabling powerful negative constraints (e.g., `T & neg NotThis`).
    - **`rot` (Rotation)**: Reverses the direction of the check, providing a tool to express contravariance and exact type matching (e.g., `T & rot T`).

- **Emergent Properties**: The orthogonal combination of these simple rules allows for the expression of complex type-level concepts like `Bottom`, `Top`, and `Exact` types from first principles.

- **Decidability by Design**: The entire system is crafted to be purely syntactic, ensuring that all constraint checks are decidable and can be efficiently executed by the compiler, avoiding the theoretical traps of semantic negation in recursive contexts.

## 🛠️ Tech Stack

- **Diagnostic Engine**: ariadne
- **Parser Generator**: lalrpop
- **Lexer Generator**: logos
- **Garbage Collection**: rust-arc-gc
- **Stack Safety**: stacksafe

## 🎓 Design Principles

Mutica's design is a synthesis of several powerful ideas:

1.  **Continuation-Passing Style**: Making control flow explicit and primary.
2.  **Coinductive Reasoning**: Providing a solid foundation for recursive types and cyclic data.
3.  **Rule-Based Constraint System**: Offering a flexible, powerful, and decidable mechanism for static analysis.
4.  **Structural Typing**: Defining type relationships through shape rather than name.

## 🤝 Contributing

Contributions are highly welcome! Please feel free to open an Issue to discuss ideas or submit a Pull Request with improvements.

## 📄 License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.

## 🔗 Resources

- [Continuation-Passing Style (CPS)](https://en.wikipedia.org/wiki/Continuation-passing_style)
- [Coinduction](https://en.wikipedia.org/wiki/Coinduction)