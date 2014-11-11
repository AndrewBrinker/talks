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
build tool and package manager), immutable-by-default variables, the trait
system, the borrow checker, and pattern matching.

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

In most languages, values are mutable by default. For example, in C++, the
following is totally valid:

```c++
int x = 5;
x = 10;
```

If you wanted to make the above invalid, you would have to declare `x` to be
`const`, thus making it immutable:

```c++
const int x = 5;
x = 10;  // Error! This isn't allowed.
```

So, C++ is mutable-by-default. For small programs this is often fine, and it has
been the standard operating procedure in popular languages for decades. However,
it has the negative effect of making it easy to accidentally mutate something
you didn't expect to be changed. After all, when you have to explicitly say that
something is immutable, it is easy to forget.

Rust avoids this by making everything immutable-by-default. So in Rust, the
following is invalid:

```rust
let x: int = 5;
x = 10;  // Error! This isn't allowed.
```

If you want to be able to mutate `x`, you have to use the keyword `mut`.

```rust
let mut x: int = 5;
x = 10;
```

This may seem like a pain, but it actually makes life easier in a number of
different ways. First, it drastically reduces the likelihood that some varaible
you've declared will change values unexpectedly during runtime. It also makes
tracking down variables that _can_ change a lot easier (just look for the ones
declared using `mut`). But the most compelling reason for this decision has to
do with concurrency.

One of the major problems in concurrency is the issue of _shared state_. When
values can modified by two threads running simultaneously it is very easy to
create _data races_, where the result is based on the order in which the
threads complete.

When variables are mutable-by-default, this can happen quite easily. If even a
single variable is modified in two threads, the possibly of data races arises.

Yet if variables are immutable-by-default, it becomes much easier to avoid the
possibility of sharing state between threads. Even better, it becomes easier for
the compiler to keep track of whether variables shared between threads are
mutable or immutable, and provide errors and warnings as necessary.

Rust goes even further by providing extremely good error messages related to
mutability mistakes. Take for example the incorrect Rust code from earlier:

```rust
let x: int = 5;
x = 10;
```

Here is what the compiler has to say about that:

```bash
main.rs:3:3: 3:9 error: re-assignment of immutable variable `x`
main.rs:3   x = 10;
            ^~~~~~
main.rs:2:7: 2:8 note: prior assignment occurs here
main.rs:2   let x: int = 5;
                ^
error: aborting due to previous error
```

The compiler noticed immediately that I was trying to reassign an immutable
variable, and told me both where the reasignment happens and where the original
happened.

## Traits

Rust is not an Object-Oriented programming language. It has no notion of classes
or objects, nor a notion of inheritance. Instead, Rust has the __trait system__.

Rust's trait system allows for the creation of generic functions, which can take
as input any type which implements the desired trait. Take for example the
following function:

```rust
fn add_one(x: int) -> int {
	x + 1
}
```

This is a function that takes in an integer, adds 1 to it, and then returns the
result. It's fairly straightforward, but only works for variables of type `int`.
If you wanted to use it on something else, you would have to define an
equivalent function for the other type. This is what happens in C. In Rust, you
can instead write a function using the trait system to accept anything that
implements the proper trait.

```rust
fn add_one<T: Add>(x: T) -> T {
	x + 1
}
```

In this case, the function accepts anything of some type `T` that implements the
`Add` trait. Internally, the Add trait defines a function like so:

```rust
pub trait Add<RHS, Result> {
    fn add(&self, rhs: &RHS) -> Result;
}
```

Where `RHS` is the type of the thing being added, the `Result` is the type of
the result of the addition. So anything that wants to allow adding simply has
to implement the add trait like this:

```rust
struct Foo;

impl Add<Foo, Foo> for Foo {
    fn add(&self, _rhs: &Foo) -> Foo {
      println!("Adding!");
      *self
  }
}

fn main() {
  Foo + Foo;
}
```

In this case, the code will print `Adding!` once, as the `add()` function is
called once by the `+` operator.

One thing to note is that as of now, Rust's trait system is not as powerful as
the C++ template system (which is actually a Turing complete metaprogramming
language). However, it is sufficiently expressive for what is needs to do, and
provides a strong basis for the creation and usage of generic functions.

(Incidentally, the Rust trait system borrows heavily from the Haskell typeclass
system, and is also inspired by C++'s once-proposed "concepts" system, which
was never implemented in the language.)

## The Borrow Checker

There is a concept in C++ (and other languages too, although it started in C++)
called RAII. It stands for "Resource Acquisition Is Initialization" and exists
to allow for the exception safe construction and destruction of objects.

Essentially, RAII says that an object is constructed when it is acquired, and
destructed when nothing owns it anymore. This way memory isn't leaked because
someone forgot to deallocate. It all happens automatically.

This is codified in Rust as the _lifetime system_. Rust tracks how long
everything in the language exists, and will automatically allocate and
initialize values when they are assigned to variables, and automatically
deallocate them when their owner goes out of scope, passing along ownership
as necessary in the meantime.

This means that Rust has to be very careful about tracking when variables
own values, which is why the __Borrow Checker__ exists. Let's look at some code:

```rust
fn dangling() -> &int {
    let i = 1234;
    return &i;
}

fn add_one() -> int {
    let num = dangling();
    return *num + 1;
}

fn main() {
	add_one();
}
```

This code fails to compile with the following error message:

```bash
temp.rs:3:11: 3:13 error: borrowed value does not live long enough
temp.rs:3     return &i;

temp.rs:1:22: 4:1 note: borrowed pointer must be valid for the anonymous lifetime #1 defined on the block at 1:22...
temp.rs:1 fn dangling() -> &int {
temp.rs:2     let i = 1234;
temp.rs:3     return &i;
temp.rs:4 }

temp.rs:1:22: 4:1 note: ...but borrowed value is only valid for the block at 1:22
temp.rs:1 fn dangling() -> &int {
temp.rs:2     let i = 1234;
temp.rs:3     return &i;
temp.rs:4  }
error: aborting due to previous error
```

What this means is that the value `1234` created inside the `dangling()`
function only exists until the end of the function, when it is automatically
destroyed. So when you try to give out a reference to it, you're giving out
a reference to something that no longer exists. In other language (like C++)
this example will lead to the dereferencing of uninitialized memory, which is a
security hole. In Rust, the borrow checker sees that you're handing out a dead
reference, and stops you from doing it.

But what if you really wanted to do it this way? Well, with a small change to
the code you can! Here it is:

```rust
fn dangling() -> Box<int> {
    let i = box 1234;
    return i;
}

fn add_one() -> int {
    let num = dangling();
    return *num + 1;
}

fn main() {
	add_one();
}
```

In this case, the `dangling()` function was modified to use `box`. This means
that `i` is now a unique pointer to the memory location containing `1234`. When
the function terminates, it transfers ownership of that pointer to the caller
(in this case `num` inside the `add_one()` function). When `add_one()`
terminates, `num` goes out of scope, and the memory it points to is reclaimed.
