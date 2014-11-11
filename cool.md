# 5 Cools Things About Rust

## Introduction

Rust is a safe systems language designed by Mozilla as an alternative to C and
C++. It is already being used in the creation of its own compiler (rustc), and
in the creation of a web browser rendering engine (Servo).

Rust focuses on providing language features that guarantee the nonexistence of
certain classes of common errors, without impairing program performance. Those
errors include:

- Memory leaks
- Data races
- Buffer overflows
- Segmentation faults

In this small essay I will walk through a collection of five Rust features and
tools that make creating systems in Rust a joy. They are: Cargo (the Rust
build tool and package manager), the borrow checker, the trait system, the
lifetime checker, and pattern matching.

## Cargo

Cargo is Rust's build tool and package manager, and while it's still fairly new
it is already quite good at its job. To start a new library in Rust, simply
type:

```bash
cargo new <name>
```

If you want your project to build into a binary instead, type:

```bash
cargo new <name> --bin
```

In either case, Cargo will automatically generate your necessary file structure,
setup a Git repo, and create a skeleton configuration file to enable you to
build your code right away.
