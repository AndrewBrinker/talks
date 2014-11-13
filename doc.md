% Haskell: A Fun, Friendly, Fantastic Functional Language
% Andrew Brinker

Hello,

Welcome to the wonderful world of Haskell. Haskell is my personal favorite
programming language, and is the most exciting and interesting language I've
ever encountered. It sometimes has a reputation for being academic or math-y.
In this introductory workshop I will attempt to show you just how untrue that
is. By the end you should be able to make simple (but useful!) programs using
Haskell, and you should have a basic understanding of the techniques used to do
so. If at any point you have a question about something I've said. Please don't
hesitate to ask!

Thank you,
Andrew Brinker

---

## What is Haskell?

Before we get into how to actually build stuff in Haskell, you should have at
least a basic understanding of what Haskell is, and how it differs from the
programming languages you're likely to already know.

Haskell is a lazy, purely functional programming language. This means a few
things:

### Laziness: Last-Minute Evaluation

One of the ways that Haskell improves the performance and expressiveness of code
is through __laziness__. This means that nothing you write in Haskell is
evaluated until it has to be. Let's look at an example from GHCi, the Haskell
interpreter:

```haskell
> let x = 3 + 7
> x
10
```

You might think that the `3 + 7` expression is evaluated in the first line, but
it's not. In fact, evaluation doesn't happen until the second line. The first
line is instead a promise to evaluate `x` to that particular value when it's
needed. Until `x` is called for, the evaluation doesn't happen, and if `x` is
never called for, the _evaluation never happens at all_.

This is laziness, and while it may seem odd, it actually allows for some pretty
cool things. Lets say you want to define a list of all odd integers.

```haskell
> let odd = [a | a <- [1..], a `mod` 2 == 1]
```

Don't worry about the syntax right now. Just know that this lazily creates a
list of every single odd number, to be evaluated one-by-one whenever you need
it. In other languages, this would be impossible, because everything is
evaluated immediately, and defining a list of infinite items will never halt.
In Haskell, so long as you don't do any operations that attempt to get the whole
list (like taking the sum of all its members, for example), you're fine. So, if
you want the first ten odd numbers:

```haskell
> take 10 odd
[1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
```

Viola! It works!

### Pure Functional Programming: A New Paradigm

Functional programming is a programming paradigm focused on the application of
functions to inputs and the avoidance of state. This means that instead of
stating __how__ something is done, you instead describe __what__ should be done.

Let's look at an example. To take the sum of all the values in a list of
integers, you might do this:

```c++
int sum(std::list<int> l) {
	int s = 0;
	std::list<int>::iterator it = l.begin();
	while (it != l.end()) {
		s += *it;
		++it;
	}
	return s;
}
```

This describes how to take the sum, which in this case is done by iterating
through the elements in the list, at each turn adding to a running total. Let's
look at the same function in Haskell:

```haskell
sum :: (Num a) => [a] -> a
sum [] = 0
sum (head:rest) = head + sum rest
```

This instead defines the function recursively, saying that the sum of a list is
the value of the first element plus the sum of the rest of the list. It performs
the same exact task as the C++ function, but doesn't actually explain how the
iteration through the list should be done. Instead, it defines an almost
obvious truth about sums, which turns out to be completely valid code!

But what does it mean to be "purely functional"? Well, in this context "pure"
just means that the function returns the same output for a certain input
_no matter what_. If you write a function like this:

```haskell
addOne :: (Num a) => a -> a
addOne x = x + 1
```

And call it like this:

```haskell
addOne 5
```

It will always give the same result (`6`). This may seem stupid, but imagine a
function in C++ that claims to do the same thing:

```c++
int addOne(int x) {
	std::ifstream input_file("blah.txt");
	if (input_file.good()) {
		return x + 2;
	}
	return x + 1;
}
```

In this case, the function tries to open a file. If the file exists and is
opened successfully, the function returns `x + 2` instead of `x + 1`. And while
it's unlikely that anyone would write such a stupid function, there's nothing in
the language to stop them from doing it, or tell you they've done it without
you cracking open the code and reading it.

In Haskell, trying to do the same thing (opening a file and optionally returning
`x + 2`) is impossible. Simply adding file handling to the function changes the
function entirely, so that it can't be called the same way. Essentially, the
compiler forces the programmer to make sure that the function is
_referentially transparent_, or 'pure' so that it always behaves predictably.

### What It Is

So now, when I say that Haskell is a lazy, purely functional programming
language, you hopefully have a better appreciation of what that means. If not,
let's continue anyway. Hopefully some more examples make it clear.

## Types, Types, Types

There are few things in Haskell more important than __types__. I'm sure you're
familiar with the idea of types, like `int` or `bool` or `char` in C++. They
provide a way for the compiler to make sure that the code you're writing makes
sense (it wouldn't make sense, for example, to write `2 + true`). Put another
way, they allow the compiler to make sure your program is behaving according to
some expected static semantics (or, compile-time meaning).

Types in Haskell are a lot stronger and cooler than types in C++ (please C++,
don't be mad at me for saying so. I still sorta like you). First of all, you
don't have to say them, the compiler automatically figures them out (except in
rare cases where it can't). Second, in Haskell functions are just another type
of variable, which lets you do things like pass functions to functions and
return functions from functions, all in a type-safe, compiler-checked way.
Third, thanks to typeclasses (which we'll cover later), you can write generic
functions which accept a variety of input types, and still guarantee that your
code is safe.

In general, all of this awesomeness means that it can be a little harder to get
Haskell code to successfully compile than it is in other languages, but it also
means that once your code compiles you can feel pretty sure that it is correct.
Put another way, Haskell takes errors that would otherwise be runtime errors,
and moves them to compile time (trust me, this is a lot better).

The basic types in Haskell are:

- `Int`: The basic integer type, whose size is based on the current machine.
- `BigInt`: Arbitrary size integer, which can grow (theoretically) infinitely.
- `Float`: 32-bit floating point number, as defined by the IEEE
- `Double`: 64-bit floating point number, also defined by the IEEE
- `Bool`: True or False. Not much more to say.
- `Char`: A Unicode text character.

These types should seem relatively familiar, as they are roughly analogous to
the basic types in C and C++. To get a better feel for them, let's fire up
GHCi (the Glasgow Haskell Compiler interpreter) with the `ghci` command.

Each type has certain operations that can be performed on it. For example,
all numeric types can be added, substracted, multiplied, and divided, like so:

```haskell
> 3 + 5
8
> 3.5 - 4.2
-0.7000000000000002
> 4 * 9
36
> 10 / 2
5
```

Boolean types can be ANDed and ORed and all that:

```haskell
> True && False
False
> False && not True
False
> False || True
True
```

Strings (defined as lists of characters), can be concatenated:

```haskell
> "Hello, " ++ " world!"
"Hello, world!"
```

In every case, these operations are essentially what you would expect, except
that these operators are nothing more than nice looking syntax on top of
Haskell's normal function system. Here is some math, written a different way:

```haskell
> (+) 5 2
7
```

In this case, `(+)` is the function name (the parentheses make Haskell treat it
as a prefix operator, rather than the usual infix one), and `5` and `2` are the
parameters. Notice than in Haskell there are no parentheses or commas needed to
pass something into a function.

Let's try some operations with incorrect types and see what happens:

```haskell
> 2 + True
<interactive>:2:3:
    No instance for (Num Bool) arising from a use of ‘+’
    In the expression: 2 + True
    In an equation for ‘it’: it = 2 + True
```

We'll get to what `(Num Bool)` means soon, but for now just notice that the
compiler immediately saw the operation was invalid, and provided a nice error
message explaining exactly what went wrong. This is a wonderful thing, and
something that Haskell is very good at.

## Functions (Which Are Also Types)

So, we've talked a bit about the basic types, now it's time to talk about
functions. Given that Haskell is a _function_al programming language, you can
rest assured that functions are extremely important for writing good,
real-world Haskell programs. Let's start with a simple example of what a
Haskell function looks like:

```haskell
addOne :: Int -> Int
addOne x = x + 1
```

You say this function earlier (albeit in a slightly different form), but we
didn't actually talk about what's going on here. Let's walk through it. The
first line is the _type annotation_. It tells the Haskell compiler what the
type of this particular function is. In this case, `addOne` is a function
which takes in an integer and returns an integer. Makes sense, right?

The next line is the actual implementation of the function. It says the
function name again, and then lists the parameter (just `x` in this case). Then
it has an equal sign, and the actual function body.

To better illustrate some of what's happening here, let's see another one:

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

In this case, we have two parameters, `x` and `y`, and both are integers. If you
look at the type annotation though, you'll notice that the syntax for one
parameter and a return value is the same as the one for two and a return value.
In fact, the only thing that indicates which one is the return value is the
fact that it comes last!

In reality, all functions in Haskell actually take _only one parameter_. When
you write something like the function `add` above, you're actually writing
something more like this:

```haskell
add :: Int -> (Int -> Int)
add x = \y -> x + y
```

This is a function that takes in a single parameter `x`, and then returns
another function that takes a parameter `y`, which then returns a single
integer value.

This process is called __currying__ (named after logician Haskell Curry, for
whom Haskell is named), and allows you to do some pretty cool things, like this:

```haskell
> let add x y = x + y
> let addTen = add 10
> addTen 5
15
```

In this example (which is back in the Haskell interpreter), we remade our `add`
function without the type annotation (which is optional). Then we made a new
function by passing only a single parameter to `add`. This function takes in
a single parameter of its own, and then adds 10 to it! Just like that, we took
a generic function we had and made a new one like it was nothing. That's the
power of currying.

That's not the only cool thing you can do with functions. Take a look at this:

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = fx : map f xs
```

What's this? This is a classic function called `map`, which is used all the time
in functional programming. Here's an example of it in use:

```haskell
> map (* 2) [1,2,3,4]
[2,4,6,8]
```

Basicaly, it takes a function (like `(* 2)`) and applies it one-by-one to each
item in a list (like `[1,2,3,4]`).

What? It takes a function as a parameter? Yes. It does. Functions in Haskell are
just like anything else. They have a type, and they can be passed around to
eachother however you like. Let's look at that `map` implementation again:

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = fx : map f xs
```

The body is doing some stuff we haven't covered yet, but the type annotation is
pretty straightforward. It takes in two parameters, one is a function from type
`a` (which can be anything) to type `b` (which can also be anything), the other
is a list of values of type `a`. It then converts those values into values of
type `b`.

In the particular case we showed, `a` and `b` are both `Int`. But they don't
have to be, and they don't have to be the same thing. Here's another example:

```haskell
> map show [1,2,3,4]
["1","2","3","4"]
```

This time, the `show` function converted each of the numbers into strings. So
`a` is `Int`, but `b` is `String`.

Learning to read type annotations is important in Haskell, and we'll be
practicing it throughout these lessons. If you don't quite understand what we've
already covered, feel free to go through it again.


