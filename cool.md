# 5 Cools Things About Rust

## Introduction

Rust is a safe systems language designed by Mozilla as an alternative to C and
C++. It is already being used in the creation of large software systems,
including its own compiler (rustc), and a web browser rendering engine called
Servo.

Rust focuses on providing language features that guarantee the nonexistence of
certain classes of common errors, without impairing program performance. Those
errors include:

- Memory leaks
- Data races
- Buffer overflows
- Segmentation faults

In this small essay I will walk through a collection of five Rust features and
tools that make creating systems in Rust a joy. They are: Cargo (the Rust
build tool and package manager), the mutability system, the trait system, the
lifetime system, and pattern matching.

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
setup a Git repo, and create a skeleton configuration file (`Cargo.toml`) to
enable you to build your code right away.

Cargo also gives handy commands to build and run your code:

```bash
cargo build
cargo run
```

Both commands will automatically fetch the dependencies defined in the
`Cargo.toml` file and build your code with them, and the second one will then
automatically execute the generated binary (if you didn't make a binary repo,
then this is all moot).

If you want to make sure that dependency versions are kept consistent between
contributors to a project, just use Cargo's auto-generated `Cargo.lock` file.
The first time a build succeeds, Cargo generates this file to keep track of
which version each dependency was on. Then in future builds it will continue to
use that exact version until it is explicitly told to update via:

```bash
cargo update <dependency name>
```

This way, all contributors on a project are guaranteed to build using the exact
same versions of all dependencies, and changes in versions are saved in the Git
repository's history (making them easy to track).

Cargo also has a nice system for running tests. With the following command,
Cargo will automatically compile your code with tests included, and then run
all the tests you've defined and report on their success:

```bash
cargo test
```

Rust itself has language-level support for testing, and Cargo makes using those
tests you've defined extremely easy.

For all of these reasons, Cargo is an excellent part of the Rust ecosystem, and
one which any real-world Rust project would be wrong to do without.

## Mutability

Test
