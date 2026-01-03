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
- 🛡️ **Meta-level Type Operators**: Unique operators like `sub` and `constraint` that manipulate constraint validation at the type level.
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
let constraint x: nat = 42;

// Character
let constraint c: char = 'A';

// Tuple
let constraint pair: (nat, nat) = (1, 2);

// AnyOf Type
let constraint value: (nat | char) = 42;

// AllOf Type (used for records/structs)
let constraint point: { x::nat & y::nat } = { x::1 & y::2 };

// Top Type (any)
let constraint anything: any = 42; // `any` is the supertype of all conventional types
let constraint anything: _ = 42;             // An underscore can be used directly to assert a type constraint
```

### Function Definitions

```mutica
// A function that accepts an integer
let constraint add_one: any = (x: nat) => x + 1; // `=>` defines a function

// A recursive function using `dyn_rec` with pattern matching
let constraint fib: any = dyn_rec f: match
    | assert 0 => 0
    | assert 1 => 1
    | constraint n: nat => f(n - 1) + f(n - 2)
    | panic; // Asserts that the match is exhaustive for the input `n: nat`
```

### Constraint Checks (`is`)

The `is` operator is not traditional subtyping, but a check to see if a type fulfills the constraints of another.

```mutica
// A value fulfills the constraint of its general type
1 is nat                           // true

// A more specific record fulfills the constraint of a more general one
{ x::1 & y::2 } is x::nat      // true
```

### Namespaces and ADTs

```mutica
// Define labeled constructors
let constraint Just: any = constraint T: any => Just::T;
let constraint Nothing: any = Nothing::();

// Define the Maybe type using a union
let constraint Maybe: any = constraint T: any => (Just T | Nothing);

// Use pattern matching on labeled types
let constraint map: any = constraint v: Maybe(any) => constraint f: lambda => 
    match v
        | constraint Just::(x: any) => Just(f(x))
        | assert Nothing::() => Nothing
        | panic;
```

### Struct-like Representation

```mutica
// Use intersection types to simulate a struct
let constraint Point: any = (x: nat, y: nat, z: nat) => { x::x & y::y & z::z };

let constraint p: any = Point(1, 2, 3);

// Deconstructuring
let constraint { x::(x: nat) & z::(z: nat) } = p;
```

### Effect Handlers

```mutica
// Define effect handlers
let constraint handler: any = match
    | assert GetA::() => 42
    | assert GetB::() => 84
    | panic;

// Use effects with handlers
handle constraint z: nat = 1 with handler;
let constraint x: nat = perform! GetA::();
let constraint y: nat = perform! GetB::();
x, y, z  // Results: 42, 84, 1
```

### Module System

```mutica
// Import from another file
let constraint pkg: any = import "lib/maybe.mu";
// Destructure imported values
let constraint {
    Just::(Just: any) &
    Nothing::(Nothing: any) &
    map::(map: any)
} = pkg;

// Use imported functions
let constraint v1: any = Just(41);
map(v1)(x: nat => x + 1)  // Results: Just(42)
```

### UFCS

```mutica
let constraint {
    Just::(Just: any) &
    map::(map: any)
} = import "lib/maybe.mu";
// Using UFCS to call functions as methods
let constraint v1: any = Just(41);
v1.map(x: nat => x + 1)  // Results: Just(42)
```

## 🎯 Example Programs

### Fibonacci

```mutica
let constraint fib: any = dyn_rec f: match
    | assert 0 => 0
    | assert 1 => 1
    | constraint n: nat => f(n - 1) + f(n - 2)
    | panic;

fib(10) // Computes the 10th Fibonacci number
```

### IO Example

```mutica
let constraint List: any = constraint T: any => rec list: (() | (T ~ list));
let constraint print_chars: any = dyn_rec print_chars: constraint str: List(char) =>
    match str
        | assert () => ()
        | constraint (head: char ~ tail: any) => {
            discard print!(head);
            print_chars(tail)
        }
        | panic;

print_chars("Hello, world!\n")
```

### Custom CPS

```mutica
let constraint println::(println: lambda) = import "lib/string.mu";
let constraint Pointer: any = (nat, nat);
let constraint alloc: lambda = constraint f: lambda => constraint v: any => {
    // RAII automatic memory management
    let constraint pointer: Pointer = alloc! v;
    let constraint result: any = f pointer;
    discard dealloc! pointer;
    result
};

let constraint my_str: Pointer = #alloc "Hello, World!";
discard println! my_str; // (0, 0) (represents as (Unit, Unit) internally)
discard println(get! my_str);
discard set!(my_str, "Goodbye, World!");
discard println(get! my_str);
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
6.  **Execution**: Linear scheduler evaluates the reduced type with effect handling

## 🤝 Contributing

Contributions are highly welcome! Please feel free to open an Issue to discuss ideas or submit a Pull Request with improvements.

## 📄 License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.
