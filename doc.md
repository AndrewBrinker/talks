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

## Functions

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
map f (x:xs) = f x : map f xs
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
map f (x:xs) = f x : map f xs
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

## GHC: The Glasgow Haskell Compiler

We've covered a lot already. Before continuing on, let's go through the steps of
making a simple Haskell program and compiling it using the Glasgow Haskell
Compiler. You'll need these steps in the upcoming sections.

First, Haskell programs are defined in files with the `.hs` extension. So, to
start a new Haskell file, type something like `touch main.hs`. This will create
your empty file to begin editing. Next open it up in your favorite text editor,
and type this:

```haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

This is just a basic Hello World program, like you would see in any other
language. Save the file, and then compile it with `ghc --make main.hs`. You can
then run the generated file with `./main`. Run it, and make sure it prints
out `Hello, World!`.

That's how you use the Glasgow Haskell Compiler. We'll be using it throughout
the remainder of the workshop, so make sure it's running correctly and your code
compiled and ran successfully. If it didn't, ask for help.

## Pattern Matching

Let's look at that `map` function we defined earlier:

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
```

This uses something called __pattern matching__. Basically, the function defines
two unique patterns, and adjusts its behavior accordingly. The first pattern
says that any function mapped over an empty list results in an empty list. The
second one says that any function mapped over a list with at least one element
is the same as the result of applying the function to the first element, and
then mapping it over the rest.

Let's look at how that would work in this case:

```haskell
> map (* 2) [1,2,3]
```

We already said what would happen from running this code, but let's actually
walk through it:

```haskell
map (* 2) [1,2,3]                           -- Pattern: map f (x:xs)
(* 2) 1 : map (* 2) [2,3]                   -- Pattern: map f (x:xs)
(* 2) 1 : (* 2) 2 : map (* 2) [3]           -- Pattern: map f (x:xs)
(* 2) 1 : (* 2) 2 : (* 2) 3 : map (* 2) []  -- Pattern: map _ []
(* 2) 1 : (* 2) 2 : (* 2) 3 : []            -- Eval: map (* 2) [] -> []
(* 2) 1 : (* 2) 2 : 6 : []                  -- Eval: (* 2) 3      -> 6
(* 2) 1 : 4 : 6 : []                        -- Eval: (* 2) 2      -> 4
2 : 4 : 6 : []                              -- Eval: (* 2) 1      -> 1
2 : 4 : [6]                                 -- Eval: 6 : []       -> []
2 : [4,6]                                   -- Eval: 4 : [6]      -> [4,6]
[2,4,6]                                     -- Eval: 2 : [4,6]    -> [2,4,6]
```

This is what is looks like when the initial code `map (* 2) [1,2,3]` is
evaluated. At each stage, the map function matches the current input against
the defined patterns, and runs the code accordingly. Once `map` application is
done, the multiplication begins to happen, following by the concatenation. In
the end, you have a list that is simply the original one with each element
multiplied by 2.

Functions can pattern match in many ways. Here is another example:

```haskell
lucky :: Int -> IO ()
lucky 5 = putStrLn "Winner!"
lucky _ = putStrLn "Meh"
```

In this case, the function `lucky` will print "Winner!" when the input is 5,
and "Meh" the rest of the time. It does this by matching the input against a
particular value. This works just the same as the last example did.

## Guards

When you want to text certain conditionals in Haskell, you can use guards, which
are similar to pattern matching. Here is an example:

```haskell
biggerest :: Int -> Int -> Int
biggerest a b
  | a >= b    = a
  | otherwise = b
```

This function will return the "biggerest" of two numbers, and it uses guards
to do it. In this case, you test conditionals (like you would in C++ with if and
else), and execute particular code accordingly.

You can also temporarily bind variables in guards as well, like so:

```haskell
bmiTell :: Float -> Float -> IO ()
bmiTell weight height
    | bmi <= 18.5 = putStrLn "You're a bit under your healthy weight."
    | bmi <= 25.0 = putStrLn "You're at a healthy weight."
    | bmi <= 30.0 = putStrLn "You're treading into unhealthy territory."
    | otherwise   = putStrLn "You have a seriously unhealthy BMI."
    where bmi = weight / height ^ 2
```

In this case, the function temporarily defines the variable `bmi`, which is more
like a constant because it can't be reassigned, and uses it in the guard
conditionals to avoid having to write (or do) the calculation multiple times.

## Let

You can also declare variables elsewhere using `let` clauses, like so:

```haskell
hello :: String -> String
hello name =
	let phrase = "Hello "
	in  phrase ++ name
```

Of course, this is a trivial example, but it shows what I'm talking about. `let`
is used to instantiate variables. It can also be used to instantiate functions,
like so:

```haskell
> [let square x = x * x in (square 5, square 3, square 2)]
[(25, 9, 4)]
```

This uses the square function to then create a list containing a single tuple
of three numbers.

`let`, like most of Haskell, is very flexible, and can be used in a variety of
contexts. Experiment with it, and see what it can or cannot do.

## Lambdas

Next up is Lambdas, which are just anonymous functions, and are written like
this:

```haskell
> map (\x -> x^3) [1,2,3]
[1,8,27]
```

This created a function without a name, and it used a special syntax to do it.
With lambdas, you start with the backslack, and then list the parameters, and
finished it off with the arrow and function body. Lambdas are really only
used in places where it's not worth it to define a separate function, and they
otherwise behave identically to functions.

## Typeclasses

This is the final big thing before we make a real Haskell program: typeclasses.
