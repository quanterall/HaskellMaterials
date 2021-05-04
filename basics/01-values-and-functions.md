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

Functions are written much like values, but have arrows in their type. The types, starting at the
leftmost, represent the arguments/parameters passed to the function, with the last (rightmost) one
representing the return value.

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

## Partial application

When you apply a function, you can choose to **not** pass all the arguments it's expecting. This
will result in the return value being a function that expects the remaining arguments and that will
have the same return value:

```haskell
addInts :: Int -> Int -> Int
addInts a b = a + b

add42 :: Int -> Int
add42 = addInts 42
```

Passing only one arguments to `addInts` in the example above results in a function that expects one
more argument instead of the original two.

It's quite common to use this fact by putting important arguments in the last parameter position of
a function in order to let people use this pattern:

```haskell
import qualified Data.List as List
import Prelude

-- | Adds 42 to every item in a list
add42 :: [Int] -> [Int]
add42 = List.map (+ 42)
```

In the above example, `List.map` takes the list it is working with as the last argument, meaning we
can just partially apply it and still get the function we expect. Since we are not passing the list
argument to `List.map` here we get a function that expects a list of integers and will return one.

We are also partially applying our `+`. The function that we are expected to pass to `List.map` is
expected to be of type `Int -> Int`, which is what we get when we write `(+ 42)`.

## Pipelines using partial application

The above pattern also works well when composing different functions to achieve an end result:

```haskell
import Control.Category ((>>>))
import Data.Function ((&))

dataPartLength :: String -> Int
dataPartLength = length . takeWhile (/= '1') . reverse

dataPartLength' :: String -> Int
dataPartLength' = reverse >>> takeWhile (/= '1') >>> length

dataPartLength'' :: String -> Int
dataPartLength'' string = reverse string & takeWhile (/= '1') & length
```

In the above examples we are pipelining functions that operate on the result of a previous function
call. The first example does this using the `.` operator, which represents classic function
composition. One thing to note about this is that the application order is read from right to left,
so we are applying `reverse` first, then `takeWhile` (to get characters that aren't '1'), then get
the length of the resulting string.

The example using `>>>` does the same thing, but can be read from left to right. The last example is
the same as commonly used pipeline operators like `|>` from F#, Elm & Elixir, and might be more
readable to some. The difference is that it requires a value on the left side, which means we have
to name our `string` value here at first. The difference is minor and I would argue that while it's
quite common to see the first example, one should strongly consider whether or not those versions
are written more because of tradition than anything else.

## A "Project Euler" example

Let's look at the very first [Project Euler](https://projecteuler.net/problem=1) as an example of
using function composition and partial application to get the answer to a mildly complex question:

> If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
> The sum of these multiples is 23.
>
> Find the sum of all the multiples of 3 or 5 below 1000.

Let's first look at the example:

```haskell
import Data.Function ((&))

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy x divisor =
  -- Note how we surround the function in backticks (`) to be able to put it in the infix position.
  let remainderOfDivision = x `rem` divisor
  in remainderOfDivision == 0

solution :: Int
solution =
  [1,2,3,4,5,6,7,8,9] & filter (\x -> x `isDivisibleBy` 3 || x `isDivisibleBy` 5) & sum -- 23
```

We can use a shorthand plus `takeWhile` to make this a bit neater:

```haskell
import Data.Function ((&))

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy x divisor =
  -- Note how we surround the function in backticks (`) to be able to put it in the infix position.
  let remainderOfDivision = x `rem` divisor
  in remainderOfDivision == 0

solution :: Int
solution =
  -- `[1..]` means "create an infinite list of increasing numbers starting from 1"
  -- We never have an infinite list in memory, but rather take elements one by one until we reach 10
  [1..] & takeWhile (< 10) & filter (\x -> x `isDivisibleBy` 3 || x `isDivisibleBy` 5) & sum -- 23
```

If we now set an upper bound as a parameter we can get the solution to the actual problem:

```haskell
import Data.Function ((&))

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy x divisor =
  -- Note how we surround the function in backticks (`) to be able to put it in the infix position.
  let remainderOfDivision = x `rem` divisor
  in remainderOfDivision == 0

solution :: Int -> Int
solution upperBound =
  -- You could also write `[1..(upperBound - 1)]` and skip `takeWhile`, but for reasons explained
  -- later on this doesn't make a difference in the execution of this function.
  [1..] & takeWhile (< upperBound) & filter (\x -> x `isDivisibleBy` 3 || x `isDivisibleBy` 5) & sum

-- `solution 1000` will give us the value 233168
```
