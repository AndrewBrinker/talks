# Introduction to Rust

## To Be Written

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

### Tuples

Tuples are heterogeneous collections of data types, where individual fields do not have names. For example:

```rust
let x = (1i, true);
let y: (int, bool) = (5i, false);
```

Once again, type annotations are optional.

Tuples (and in fact all compound types) can be destructured during assignment, like this:

```rust
let (x, y): (int, int) = (1i, 5i);
println!("{} {}", x, y);
// Prints: 1 5
```

In this case, x is assigned to the first field in the tuple, and y to the second. This is called _destructuring_, because it breaks apart the structure of the compound type to access the internel values directly.

### Structs

Structs are similar to tuples, except their fields have names. They are declared like this:

```rust
struct Point {
  x: int,
  y: int
}
```

And can be used like this:

```rust
struct Point {
  x: int,
  y: int
}

fn main() {
  let p: Point = Point {x: 1i, y: 5i};
  println!("{} {}", p.x, p.y);
  // Prints: 1 5
}
```

In this case, the fields are initialized and accessed by their name, rather than by their position.

### Enums

Enums are the final compound type, and they are substantially different from tuples and structs. Tuples and structs are known as _product types_ because their type is the product of their member's types. So a tuple with two ints has type (int, int), or a structure with two ints has type {int, int}. In either case, their final type is essentially the logical AND of the two member types.

Enums are known as _sum types_, and their type is instead the logical OR of the member's types. Take this for example:

```rust
enum Status {
  Alive,
  Dead
}
```

Something of type Status can be either Alive or Dead, but not both (no zombies here).

Enums can also have fields in their variants, like so:

```rust
struct Point {
  x: int,
  y: int
}

struct Length {
  l: int
}

enum Shape {
  Circle(Point, Length),
  Rectangle(Point, Length, Length)
}
```

This is sort of a silly example, but it shows that enums variants can each have their own fields. Essentially, enums allow for the selection of one of several tuples.

## Pattern Matching

One of the major features of Rust is called __pattern matching__, which happens using the `match` statement:

```rust
let x = std::io::stdin.read_line().ok().expect(); // We'll cover this soon.
match x {
  x > 10  => println!("Greater than 10!"),
  x == 10 => println!("Equals 10."),
  _       => println!("Less than 10...")
}
```

Essentially, `match` is a beefed-up version of the `switch` statement from C and C++. It defines a series of cases, along with operations to be performed in each case. However, in Rust, __matches must be exhaustive__. If we modify the above example like so:

```rust
let x = std::io::stdin.read_line().ok().expect(); // We'll cover this soon.
match x {
  x > 10  => println!("Greater than 10!"),
  x == 10 => println!("Equals 10.")
}
```

It no longer compiles! This is because the compiler requires that any match cover every possible case (which is we used the `_` case earlier. It matches everything).

### Pattern Matching & Compound Types

Combined with compound data structures, pattern matching allows for some very interesting code:

```rust
enum OptionalInt {
  Value(int),
  Missing
}

fn main() {
  let x = Value(5);
  let y = Missing;

  match x {
    Value(n) => println!("x is {}", n),
    Missing  => println!("x is missing!")
  }

  match y {
    Value(n) => println!("y is {}", n),
    Missing  => println!("y is missing!")
  }
}
```

This `OptionalInt` business may not seem like much, but imagine a function that may either return an integer or nothing (depending on whether the input is valid, for example). In other language it is very easy to forget the check if the result is nothing, and forgetting that can create some real problems down the line for your software.

In Rust, if your function returns an `Option<int>` (the generic form of the `OptionalInt` we just made), the compiler won't compile your code until you check whether the function returns a value or `Nothing`. A whole group of potential errors is gone!


## Strings, Arrays, Vectors, and Slices

### Strings

What would a programming language be without strings?

In Rust, there are two string types: `String` and `&str`. The first is a heap-allocated Unicode string of variable size, the second is known as a "string slice" and is a view into a statically-defined string (no allocation).

Here is an example to show what I'm talking about:

```rust
fn say_hello(name: String) {
  println!("Hello {}!", name);
}

fn main() {
  let name = std::io::stdin.read_line().ok().expect();
  say_hello(name.trim()); // Trim removes the trailing newline
  say_hello("Bart Simpson".to_string());
}
```

The call to `say_hello()` passes in a `String` without conversion. The second converts a `&str` into a `String`. Generally, it is preferable to avoid turning `&str` into `String` if you can avoid it, as that adds allocations to your programs. For this reasons many functions prefer to work with `&str` inputs.


