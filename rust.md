# Introduction to Rust

## Table of Contents

1. Making projects with Cargo
  1. `cargo new`
  2. `cargo build`
  3. `cargo run`
2. Types & Mutability
  1. Integer types
  2. Floats and Doubles
  3. Booleans
  4. Mutability
3. Control Flow
  1. If/Else
  2. For
  3. While
  4. Loop
4. Functions
  1. Implicit returns
5. Compound Data Types
  1. Tuples
  2. Structs
  3. Enums
6. Pattern Matching
  1. Non-exhaustive patterns
  2. Error handling with Option
7. Strings, Arrays, and Vectors
  1. `String` vs. `&str`
  2. Arrays
  3. Vectors
  4. Slices
8. Standard Input
  1. stdin
  2. println!
9. Guessing Game
  1. Start a new project
  2. Get user guess
  3. Generate the answer
  4. Compare guesses

## Welcome to Rust

Rust is a safe systems programming language being developed by Mozilla (makers of Firefox). It is intended to fill the same role of languages like C++ and C, while ensuring a greater degree of safety. This means that Rust is designed to minimize or eliminate things like:

- Data races
- Buffer overflows
- Dereferencing a null or dangling pointer
- Mutating an immutable value or reference
- Reading uninitialized memory
- Aliasing pointers
- Indexing outside of the bounds of an object
- Invalid values in primitive types

It does this through a powerful type system and compiler which ensure _at compile time_ that no program engages in these behaviors.

In this short introduction (adapted from the official Rust Guide, found at http://doc.rust-lang.org/guide.html) I will walk through some of the basics of Rust usage. This does not include some of the more advanced (and indeed interesting) parts of the language, but should be sufficient for anyone to make basic programs and experiment with the language. Further exploration is encouraged, and all information can be found on the official Rust website: http://www.rust-lang.org/.

## Cargo

__Cargo__ is the Rust build system and dependency manager, and is used for the creation and management of all real-world Rust projects.

### Making a New Project

Creating a new 'crate' (Cargo's term for projects) is done with `cargo new`. Crates are setup as libraries by default, but can be set to compile into binaries with the `--bin` flag. Thus, to create a new Rust project which compiles into a binary, use `cargo new <name> --bin`.

### Building the Project

Once a project has been created, you can build the project (including all necessary dependencies) using `cargo build`. This is all done based on the contents of the `Cargo.toml` file, which defines the project's configuration.

### Running the Executable

Finally, `cargo run` is available as a shorthand to build and execute the current project.

The full list of Cargo commands, and a more complete explanation of what they do can found at http://crates.io.

## Types & Mutability

Like all languages, Rust has a small collection of _primitive types_. These are the basic types of values provided by the language itself, and are the things upon which the rest of the language is built.

The basic types in Rust are:

- `int`/`uint`
- `i8`/`i16`/`i32`/`i64`/`u8`/`u16`/`u32`/`u64`
- `f32`/`f64`
- `bool`
- `char`

### Numeric Types

The first two, `int` and `uint`, are for integers and unsigned integers, respectively. Their size is based on the width of pointers on the current system, and so may vary from machine to machine.

The next ones are also for integers and unsigned integers, but they have explicit widths of 8, 16, 32, or 64 bits. Overflow and underflow on these types is well-defined as wrapping behavior.

There are also `f32` and `f64`, which are analogous to `float` and `double` in C or C++.

### Boolean Types

Then there's `bool`, Rust's basic true or false type. This is a standby in many languages, but is not commonly used in Rust due to the availability of Pattern Matching, which will be discussed later.

### Textual Types

Finally, there's `char`, which is defined as a single Unicode code-point. This is the basic building block for strings, which will also be discussed later.

### Variable Declaration

Variables in Rust are declared using the `let` keyword, like so:

```rust
let x = 5;
```

Declarations do not have to have a type annotation if a type can be inferred by the compiler, but may have one optionally like so:

```rust
let x: int = 5;
```

### Mutability

All variables in Rust are __immutable by default__, which means something like the following will fail to compile.

```rust
let x = 5;
x = 10;  // Error! x isn't mutable!
```

To declare something as mutable, use `mut`.

```rust
let mut x = 5;
x = 10;  // This is just fine.
```

This mutability constraint may seem strange, but it forces the programmer to carefully consider whether values should be allowed to be modified, and restricts the possibility of mutable state, particularly in situations with multiple threads working at the same time.

## Control Flow

### If/Else

The most basic Rust control flow mechanism is the classic if/else:

```rust
if true {
  println!("It was true!");
} else {
  println!("It was false...");
}
```

However, unlike other languages if and else are expressions, not statements, and can be used like so:

```rust
let x = if true {
  5
} else {
  10
};
```

In this case, the value of x is based on the result of the conditional expression being evaluated.

### For Loops

There's also for loops, which look like this:

```rust
for x in range(0i, 5i) {
  println!("{}", x);
}
```

For loops in Rust use iterators. In this case, the range function creates an iterator which starts at 0 and goes to 4 (the integer before 5), assigning x to the value the iterator currently has each time through the loop. Thus, the above code would print:

```
0
1
2
3
4
```

This can also be used to go through arrays and similar data structures, like so:

```
let a = [0i, 1i, 2i, 3i, 4i];
for elem in a.iter() {
  println!("{}", elem);
}
```

Which will print out the same results as the previous code example.

### While Loops

There are also while loops, which work as you would expect:

```rust
// input and answer are defined somewhere previously
while input != answer {
  // Do some stuff
}
```

### Infinite Loops

If you want an infinite loop (only exited by a break), then you should use `loop` instead, because it can be nicely optimized by the compiler:

```rust
loop {
  if end {
    break;
  }
  // Otherwise, do stuff
}
```

## Functions

Functions are (unsurprisingly) important in Rust. They are defined like so:

```rust
fn add_one(x: int) -> int {
  x + 1
}
```

In this case, the function takes in one parameter `x` of type `int`, and returns a value of type `int`. In Rust, returns can be implicit, and are taken from the last line if that line doesn't end in a semicolon (which would automatically throw away the return value).

Functions without a return type can be declared like this:

```rust
fn say_hello() {
  println!("Hello!");
}
```

That is, without the arrow trailing the parameter list.

## Compound Data Types

Rust provides several different types of compound data types (types constructed as combinations of other types). They are tuples, structs, and enums.


