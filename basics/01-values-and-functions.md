# Values and functions (& basic types)

The bread and butter of a Haskell program is of course values and functions.

## Values

Before a value or a function definition comes the type of the definition. This is by convention to a
large degree, we don't actually **have to** specify types for a lot of things depending on whether
or not the compiler can figure them out itself. Even so, most projects will be set up to warn you
when a top-level definition doesn't have an explicit type for the simple reason that it's part of
good documentation to specify the intended type.

A type signature is written with the name of the thing it's for, followed by `::` and then the type:

```haskell
myCoolValue :: Bool
```

The actual definition for it comes immediately after and is written with `=` to signify that the
name on the left is equal to the expression on the right. In the case of values we don't have to
consider parameters:

```haskell
-- We can create a value of type `Bool` using either `True` or `False`
myCoolValue = True
```

The basic primitive types in Haskell are:

- `Int` (limited in size)
- `Integer` (can widen as needed, useful if you know you'll move into BigInt territory)
- `Float` (32-bit floating point)
- `Double` (64-bit floating point)
- `Bool`
- `Char`

```haskell
myAnswerToEverything :: Int
myAnswerToEverything = 42

myInteger :: Integer
myInteger = 42

myChar :: Char
myChar = '$'

myFloat :: Float
myFloat = 1337.0

myDouble :: Double
myDouble = 1337.0
```

String are actually made up out of lists of characters by default, but because `Strings` are common
we'll cover them here:

```haskell
myString :: String -- <- this is just an alias for `[Char]`
myString = "This is not the best string in the world, it's just a tribute"
```

## Functions

Functions are written much like values, but have arrows in their type. The type that comes after the
last arrow is the return type and each arrow before that signifies a new parameter to the function:

```haskell
addInts :: Int -> Int -> Int
addInts a b = a + b
```

Calling functions, as we can see above, can be done via infix notation when they are operators. We
could also call the operator in a prefix position:

```haskell
addInts :: Int -> Int -> Int
addInts a b = (+) a b
```

The application of a function is done by just writing the function name followed by a space and then
a space for each parameter, making it as lightweight an operation as possible.

```haskell
isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy x divisor =
  -- When we want to divide our problem into smaller parts we can use `let`.
  let remainderOfDivision = rem x divisor
  -- `let` has to be followed by `in` and then the final expression that is to be executed.
  in remainderOfDivision == 0
```

`rem` in the above code snippet is a function that takes an integer-like number and returns the
remainder of dividing it by the second argument. Here we check whether or not that division returns
0 to determine if it was divisible by it.

We could also write the division as follows:

```haskell
isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy x divisor =
  -- Note how we surround the function in backticks (`) to be able to put it in the infix position.
  let remainderOfDivision = x `rem` divisor
  in remainderOfDivision == 0
```

This can be used to make code more intuitive and is very common in the case of checking if something
is a member of a map, list or the like:

```haskell
element `Map.member` ourMap
```

```haskell
element `List.elem` ourList
```

This is a general facility that you can apply to any function, even functions with more than two
arguments. It's important to recognize that it's not always a great idea to use this feature and
each case should be examined on an individual basis in terms of whether or not it makes the code
more or less easy to understand.
