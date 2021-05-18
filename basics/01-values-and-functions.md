# Values and functions (& basic types)

- [Values and functions (& basic types)](#values-and-functions--basic-types)
  - [Running examples](#running-examples)
  - [Values](#values)
    - [Exercises (Values)](#exercises-values)
  - [Functions](#functions)
    - [Boolean & arithmetic operations](#boolean--arithmetic-operations)
    - [Exercises (Functions)](#exercises-functions)
      - [Exercise notes (Functions)](#exercise-notes-functions)
  - [Partial application](#partial-application)
    - [Exercises (Partial application)](#exercises-partial-application)
      - [Exercise notes (Partial application)](#exercise-notes-partial-application)
  - [Pipelines using partial application](#pipelines-using-partial-application)
  - [A "Project Euler" example](#a-project-euler-example)
  - [Asking questions about values](#asking-questions-about-values)
  - [A note on functions, their parameter order and partial application](#a-note-on-functions-their-parameter-order-and-partial-application)

The bread and butter of a Haskell program is of course values and functions.

## Running examples

If you want to run the examples, I recommend to install `stack` via the link found in the README and
using our ready-made template for small applications: `stack new project-name quanterall/basic`.
This will give you an already set-up project and you can put the example code in the `Library.hs`
file.

If you are so inclined, you can use development containers to get a functioning development
environment that I've set up; the prompt should pop up when you open the project. If you don't feel
like you want this, you can install the "Simple GHC (Haskell) Integration" (extension ID
dramforever.vscode-ghc-simple`) in VSCode for a setup that is known to work. When you save you
should be able to get compiler warnings and errors.

Most examples have (and should have) the needed imports in the examples themselves, so if you find
one that doesn't, please let me know.

## Values

Before a value or a function definition comes the type of the definition. This is by convention to a
large degree, we don't actually **have to** specify types for a lot of things depending on whether
or not the compiler can figure them out itself. Even so, most projects will be set up to warn you
when a top-level definition doesn't have an explicit type for the simple reason that it's part of
good documentation to specify the intended type.

A type signature is written with the name of the thing it's for, followed by `::` and then the type:

```haskell
myValue :: Int
```

The actual definition for it comes immediately after and is written with `=` to signify that the
name on the left is equal to the expression on the right:

```haskell
myValue :: Int
myValue = 42
```

The basic types in Haskell:

```haskell
import Prelude

myAnswerToEverything :: Int -- Integer, limited in size
myAnswerToEverything = 42

myInteger :: Integer -- Integer, can grow as needed
myInteger = 42

myChar :: Char -- A single character
myChar = '$'

myFloat :: Float -- 32-bit floating point value
myFloat = 1337.0

myDouble :: Double -- 64-bit floating point value
myDouble = 1337.0

myBool :: Bool -- `True` or `False`
myBool = False

myString :: String -- Actually just a type alias for a list of characters; `[Char]`
myString = "This is not the best string in the world, it's just a tribute"
```

### Exercises (Values)

1. Define a value with the name `myOwnValue` with the type `Double` and the value `42`.

2. Define a value with the type `String` and the value `['h', 'e', 'l', 'l', 'o']`, then a value
   with the type `String` and the value `"hello"`. Why do both of these work?

## Functions

Functions are written much like values, but have arrows in their type. The types, starting at the
leftmost, represent the arguments/parameters passed to the function, with the final (rightmost) one
representing the return value.

```haskell
import Prelude

--          a      b     result
addInts :: Int -> Int -> Int
addInts a b = a + b
```

Calling functions, as we can see above, can be done via infix notation when they are operators. We
could also call the operator in a prefix position:

```haskell
import Prelude

--          a      b     result
addInts :: Int -> Int -> Int
addInts a b = (+) a b
```

The application of a function is done by just writing the function name followed by a space and then
spaces between the parameters, making it as lightweight an operation as possible.

```haskell
import Prelude

--                x    divisor result
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
import Prelude

--                x    divisor result
isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy x divisor =
  -- Note how we surround the function in backticks (`) to be able to put it in the infix position.
  let remainderOfDivision = x `rem` divisor
   in remainderOfDivision == 0
```

This can be used to make code read more intuitively and is very common in the case of checking if
something is a member of a map, list or the like:

```haskell
key `Map.member` ourMap -- Is the key defined in the map?
```

```haskell
element `List.elem` ourList -- Is the element present in the list?
```

We can use this with any function, even functions with more than two arguments. It's important to
recognize that it's not always a great idea to use this feature and each case should be examined on
an individual basis in terms of whether or not it makes the code more or less easy to understand.

### Boolean & arithmetic operations

| Math | Haskell | Notes |
| :--: | :-----: | :------------------: |
|  +   |    +    |                      |
|  -   |    -    |                      |
|  *   |    *    |                      |
|  /   |    /    |                      |
|  >   |    >    |                      |
|  <   |    <    |                      |
|  ≥   |   >=    |                      |
|  ≤   |   <=    |                      |
|  =   |   ==    |                      |
|  ≠   |   /=    |                      |
|  ^   |    ^    | `Int` & `Integer`    |
|  ^   |   **    | `Float` & `Double`   |
| mod  |   rem   | `Int` & `Integer`[0] |
| mod  |   mod   | `Int` & `Integer`[0] |

| Math |  C   | Haskell |        Notes        |
| :--: | :-:  | :-----: | :-----------------: |
|  ¬   |  !   |   not   |                     |
|  ∧   |  &&  |   &&    |                     |
|  ∨   |  ǀǀ  |   ǀǀ    | Two pipe characters |

[0]:

```haskell
-- Note: Use `rem` if you know your parameters are both positive

5 `mod` 3 == 2
5 `rem` 3 == 2

5 `mod` (-3) == -1
5 `rem` (-3) == 2

(-5) `mod` 3 == 1
(-5) `rem` 3 == -2

(-5) `mod` (-3) == -2
(-5) `rem` (-3) == -2
```

### Exercises (Functions)

1. Define a function that returns whether or not an `Int` is zero.

2. Define a function that returns whether or not a `Float` is greater than zero.

3. Define a function that takes two `Int`s and returns the biggest of the two.

4. Define a function that takes two `Int`s and returns the smallest of the two.

5. Define a function that adds 1/10th of a Double to itself.

6. Define a function that takes 2 `Float`s `length'` & `width` and returns the area of the rectangle
   they make up.

7. Define a function that takes a radius of type `Float` and returns the area of a circle.

8. Define a function `calculateBMI` that takes a `Float` representing weight and an `Int`
   representing height in centimeters and returns the person's BMI.

#### Exercise notes (Functions)

0. `pi` is available by default.
1. `fromInteger` can be used to turn an integer into a `Float`.

## Partial application

When you apply a function, you can choose to **not** pass all the arguments it's expecting. This
will result in a function that expects the remaining arguments and that will have the same return
value:

```haskell
import Prelude

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
add42ToAll :: [Int] -> [Int]
add42ToAll = List.map (\x -> x + 42)
```

In the above example, `List.map` takes the list it is working with as the last argument, meaning we
can just partially apply it and still get the function we expect; a function that expects a list of
integer values that we then add `42` to, returning the resulting list. Since we are not passing the
list argument to `List.map` here we get exactly that function signature.

We can also partially apply our `+`. The function that we are passing to `List.map` is expected to
be of type `Int -> Int`, which is what we get when we write `(+ 42)`:

```haskell
import qualified Data.List as List
import Prelude

-- | Adds 42 to every item in a list
add42ToAll :: [Int] -> [Int]
add42ToAll = List.map (+ 42) -- could also be `(42 +)`
```

Since operators expect arguments both on the left and right side we can partially apply whichever
side we want, so `(42 +)` is also valid. The resulting behavior obviously depends on the operator,
as an operator like `-` would behave differently depending on which side you are omitting.

### Exercises (Partial application)

1. Define a function that takes a list of `Int`s and multiplies each with `2`. Remember `map`[0].

2. Define a function that takes a list of `Int`s and squares each. Remember `map`[0].

3. `filter`[1] over a list of `Int` takes a function of type `Int -> Bool`. Construct a function
   that checks whether or not the number is `0` without naming it.

4. Define a function that returns all of the `Int`s in a list over `0`, use partial application for
   both the check and `filter`[1].

5. Define a function that takes all the first numbers of a list of `Int`s below 10, stopping when it
   reaches one that does not meet that criteria. `takeWhile`[2] can be useful for this. Use partial
   application both for `takeWhile`[2] and the predicate you pass to it.

6. Define a function that takes the last 3 elements of a `[Int]`. Use notes as inspiration.

7. Define a function that checks whether or not all `Int`s in a a list are even. Use notes to figure
   out how and use partial application for your definition.

#### Exercise notes (Partial application)

0. [`map`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:map)
1. [`filter`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:filter)
2. [`takeWhile`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:takeWhile)
3. [`take`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:take)
4. [`reverse`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:reverse)
5. [`all`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:all)
6. [`even`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:even)

## Pipelines using partial application

Partial application is especially useful in pipelines, since we can construct new functions by
partially applying other functions, easily creating a function that takes the result of a previous
operation and returning a new one, possibly passing it along to yet another function:

```haskell
import Control.Category ((>>>))
import Data.Function ((&))
import Prelude

-- `/=` is the "not equal" operator in Haskell, analogous to `!=` in many other languages.
-- Note how we're again using an operator with only one argument, and are missing the left-most one,
-- which gives us a function expecting one argument, which is exactly what the predicate we pass to
-- `takeWhile` here expects: `Char -> Bool`

-- Types for this example:
--
-- reverse :: String -> String
-- takeWhile :: (Char -> Bool) -> String -> String
-- length :: String -> Int

dataPartLength :: String -> Int
dataPartLength = length . takeWhile (/= '1') . reverse

dataPartLength' :: String -> Int
dataPartLength' = reverse >>> takeWhile (/= '1') >>> length

dataPartLength'' :: String -> Int
dataPartLength'' string = string & reverse & takeWhile (/= '1') & length
```

In the above examples we are pipelining functions that operate on the result of a previous function
call. The first example does this using the `.` operator, which represents classic function
composition. One thing to note about this is that the application order is read from right to left,
so we are applying `reverse` first, then `takeWhile`, then `length`.

`reverse` takes a `String` and will reverse it. The result is then passed to `takeWhile (/= '1')`
which will take all initial characters of the string until we find one that is `'1'`. The result of
that is then passed to `length` which will return the length.

The example using `>>>` does the same thing, but can be read from left to right. The last example,
using the operator `&` is the same as commonly used pipeline operators like `|>` from F#, Elm &
Elixir, and might be more readable to some. It works by taking whatever value we have on the left
side of it and passing it to the function on the right. Like F# and Elm the value is passed as the
last argument to the function on the right, as opposed to Elixir where it is passed as the first.

While we aren't changing the meaning of our program based on which way we compose our functions, one
should consider whether or not it makes sense for the intended reader of the code to read it
left-to-right like we usually do with English and how most of our language constructs work, over the
alternative with `.` where we instead have to read the flow of things from right-to-left.

## A "Project Euler" example

Let's look at the very first [Project Euler](https://projecteuler.net/problem=1) as an example of
using function composition and partial application to get the answer to a mildly complex question:

> If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
> The sum of these multiples is 23.
>
> Find the sum of all the multiples of 3 or 5 below 1000.

Let's first look at the given example numbers in action:

```haskell
import Data.Function ((&))
import Prelude

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
import Prelude

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
import Prelude

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

If we wanted to support using different divisors we could also do the following:

```haskell
import Data.Function ((&))
import Prelude

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy x divisor =
  -- Note how we surround the function in backticks (`) to be able to put it in the infix position.
  let remainderOfDivision = x `rem` divisor
   in remainderOfDivision == 0

solution :: Int -> [Int] -> Int
solution upperBound divisors =
  -- `any` takes a predicate/question and a list of inputs and answers the question:
  -- "Are any of these true?"/"Do any of these return `True`?"
  -- Here we are asking "Is X divisble by any of the passed in divisors?"
  -- We could also write `isDivisibleBy x` only, but it can be useful to use backticks to emphasize
  -- that `x` is the first argument, and to make it read as a question about `x`.
  [1..] & takeWhile (< upperBound) & filter (\x -> any (x `isDivisibleBy`) divisors) & sum

-- `solution 1000 [3, 5]` will give us the value 233168
```

## Asking questions about values

Quite regularly we will have to pose some kind of question about the structure of a value, or
whether or not it matches some kind of predicate. Let's look at a few ways to do this in the case of
"clamping" a number:

```haskell
import qualified System.Environment as Environment
import Prelude

-- | Limits a given integer to be within the range @lowerBound <= value <= upperBound@.
clamp :: Int -> Int -> Int -> Int
clamp lowerBound upperBound value
  | value < lowerBound = lowerBound
  | value > upperBound = upperBound
  | otherwise = value

runMain :: IO ()
runMain = do
  arguments <- Environment.getArgs
  case arguments of
    [numberString] ->
      let number = read numberString
       in print $ clamp 0 255 number
    _otherwise ->
      putStrLn "Need a number to be passed as an argument"
```

The above is likely the most natural way of doing this in this very example, because we have several
questions to ask about the value and we can do so immediately in what are known as "guards".

Note that we do not have an immediate `=` after our parameters but instead each `|` introduces a new
question that we pose, a new **guard**. If the boolean expression that follows the pipe (`|`)
evaluates to `True` the expression to the right of `=` is what will be returned.

The word `otherwise` is an always matching case and we can use this case as an "for all other cases"
clause.

Another example:

```haskell
import qualified System.Environment as Environment
import Prelude

-- | Limits a given integer to be within the range @lowerBound <= value <= upperBound@.
clamp :: Int -> Int -> Int -> Int
clamp lowerBound upperBound value =
  if
      | value < lowerBound -> lowerBound
      | value > upperBound -> upperBound
      | otherwise -> value

runMain :: IO ()
runMain = do
  arguments <- Environment.getArgs
  case arguments of
    [numberString] ->
      let number = read numberString
       in print $ clamp 0 255 number
    _otherwise ->
      putStrLn "Need a number to be passed as an argument"
```

The above example shares a lot of structure with our other example. Again, the first example is
likely the most natural choice for our given problem, but that way only works when we immediately
ask questions about our input arguments and want to branch differently for the cases entirely.

What we have above is a generalized `if` that supports many branches with arbitrary guards. It's
called a "multi-way `if`" and is enabled by the `MultiWayIf` extension, which we've enabled by
default in our templates.

While we need to be aware that this does require an extension, it's still useful to learn and use
this as a standard part of the language. Why? Because it's very flexible and solves the issue of
having to introduce another, more roundabout way of being able to use guards:

```haskell
import qualified System.Environment as Environment
import Prelude

-- | Limits a given integer to be within the range @lowerBound <= value <= upperBound@.
clamp :: Int -> Int -> Int -> Int
clamp lowerBound upperBound value =
  case () of
    () | value < lowerBound -> lowerBound
    () | value > upperBound -> upperBound
    () | otherwise -> value

runMain :: IO ()
runMain = do
  arguments <- Environment.getArgs
  case arguments of
    [numberString] ->
      let number = read numberString
       in print $ clamp 0 255 number
    _otherwise ->
      putStrLn "Need a number to be passed as an argument"
```

Because `case` expressions allow us to use guards we can introduce this `case` expression context
where we don't actually pattern match on anything but still use the guards. This is obviously not
ideal and when we see this it's much better to just use multi-way `if`.

`case` expressions and their cousin, top-level pattern matching, are ideal for when we want to match
on the structure of a given value, either with literal values exactly or want to pull apart some
structure via their constructors and field names:

```haskell
import qualified System.Environment as Environment
import Prelude

runMain :: IO ()
runMain = do
  arguments <- Environment.getArgs
  case arguments of
    [xString, divisorString] ->
      let x = read xString
          divisor = read divisorString
       in print $ safeDivide x divisor
    _otherwise ->
      putStrLn "Need a number to be passed as an argument"

data DivisionResult
  = DivideSuccess Float
  | DivisionByZero
  deriving (Show)

safeDivide :: Int -> Int -> DivisionResult
safeDivide _x 0 = DivisionByZero
safeDivide x divisor =
  let xAsFloat = fromIntegral x
      divisorAsFloat = fromIntegral divisor
   in DivideSuccess (xAsFloat / divisorAsFloat)
```

We will go deeper into how to use `data` in the [next document](./02-composite-datatypes.md) but for
now all we need to know is that `safeDivide` can either return a `DivisionByZero` result or a
`DivideSuccess` result that also carries a float with it.

If we run this program we can see the following:

```bash
$ stack run -- 5 0
DivisionByZero
$ stack run -- 5 2
DivideSuccess 2.5
```

We can use `case` to immediately ask questions about this structure:

```haskell
import qualified System.Environment as Environment
import Prelude

runMain :: IO ()
runMain = do
  arguments <- Environment.getArgs
  case arguments of
    [xString, divisorString] ->
      let x = read xString
          divisor = read divisorString
       in -- Note how we use `case` here to deconstruct the result
          case safeDivide x divisor of
            DivideSuccess result ->
              putStrLn $ "Your result was: " <> show result
            DivisionByZero ->
              putStrLn "You tried to divide by zero"
    _otherwise ->
      putStrLn "Need a number and a divisor to divide it by"

data DivisionResult
  = DivideSuccess Float
  | DivisionByZero
  deriving (Show)

safeDivide :: Int -> Int -> DivisionResult
safeDivide _x 0 = DivisionByZero
safeDivide x divisor =
  let xAsFloat = fromIntegral x
      divisorAsFloat = fromIntegral divisor
   in DivideSuccess (xAsFloat / divisorAsFloat)
```

And when we run it:

```bash
$ stack run -- 5 0
You tried to divide by zero
$ stack run -- 5 2
Your result was: 2.5
```

You may have noticed that I have referred to `case` as an expression; this is not a mistake. If we
change the above code example to use the following we can see that indeed, `case` is an expression
like everything else:

```haskell
putStrLn $ case safeDivide x divisor of
  DivideSuccess result ->
    "Your result was: " <> show result
  DivisionByZero ->
    "You tried to divide by zero"
```

Here we are saying that `putStrLn` will take whatever our `case` expression returns, meaning it in
this case always will have to return a `String`. What this means is that `case` expressions have to
return values of the same type in all branches and there is no "empty case" where we return `void`
or the like.

As we saw in the previous example we can execute actions in our case branches. That example, where
we printed a string in each branch of the `case`, worked because we were constructing an
action of type `IO ()` in each branch when we executed `putStrLn ...`.

## A note on functions, their parameter order and partial application

It could feel natural to make `clamp` from our previous example take the parameters in a slightly
different order. The original:

```haskell
-- | Limits a given integer to be within the range @lowerBound <= value <= upperBound@.
clamp :: Int -> Int -> Int -> Int
clamp lowerBound upperBound value
  | value < lowerBound = lowerBound
  | value > upperBound = upperBound
  | otherwise = value
```

If one were to take the parameters as follows it might match the expression in the documentation
more clearly:

```haskell
-- | Limits a given integer to be within the range @lowerBound <= value <= upperBound@.
clamp :: Int -> Int -> Int -> Int
clamp lowerBound value upperBound
  | value < lowerBound = lowerBound
  | value > upperBound = upperBound
  | otherwise = value
```

There is a downside to doing this, however: We will not be able to partially apply the function to
very great effect:

```haskell
-- Takes an upper bound but what does it actually accomplish?
takesUpperBound :: Int -> Int
takesUpperBound = clamp 0 255

clamp :: Int -> Int -> Int -> Int
clamp lowerBound value upperBound
  | value < lowerBound = lowerBound
  | value > upperBound = upperBound
  | otherwise = value
```

As we can see, the partially applied (`clamp lowerBound value`) function will now take an upper
bound and return a result. Depending on your use case this can be unintuitive design. If we instead
take the value to clamp as the last argument we get a useful way to construct new functions:

```haskell
clampsToByteValues :: Int -> Int
clampsToByteValues = clamp 0 255

clamp :: Int -> Int -> Int -> Int
clamp lowerBound upperBound value
  | value < lowerBound = lowerBound
  | value > upperBound = upperBound
  | otherwise = value
```

In the above example we've partially applied `clamp` to `0` and `255` and what we get out of it is
a function that takes a value and correctly returns the value if it is in that range or either of
the boundaries if it is outside.

If we are using this function in a pipeline of functions, it is a lot more intuitive:

```haskell
import Data.Function ((&))
import qualified System.Environment as Environment
import Prelude

runMain :: IO ()
runMain = do
  arguments <- Environment.getArgs
  case arguments of
    [xString, divisorString] ->
      let x = read xString
          divisor = read divisorString
          -- This pipeline is basically saying to take the divisor we have, clamp it to within the
          -- range `1 <= value <= 255` then safely divide `x` by the result.
          divisionResult =
            divisor
              & clamp 1 255
              & safeDivide x
       in putStrLn $ case divisionResult of
            DivideSuccess result ->
              "Your result was: " <> show result
            DivisionByZero ->
              "You tried to divide by zero"
    _otherwise ->
      putStrLn "Need a number and a divisor to divide it by"

data DivisionResult
  = DivideSuccess Float
  | DivisionByZero
  deriving (Show)

safeDivide :: Int -> Int -> DivisionResult
safeDivide _x 0 = DivisionByZero
safeDivide x divisor =
  let xAsFloat = fromIntegral x
      divisorAsFloat = fromIntegral divisor
   in DivideSuccess (xAsFloat / divisorAsFloat)

clamp :: Int -> Int -> Int -> Int
clamp lowerBound upperBound value
  | value < lowerBound = lowerBound
  | value > upperBound = upperBound
  | otherwise = value
```

Running this we can see that we've effectively removed the issue of unsafe division by clamping the
divisor to a minimum of `1`:

```bash
$ stack run -- 5 0
Your result was: 5.0
$ stack run -- 5 2
Your result was: 2.5
```

Using `safeDivide` even in the presence of clamping here isn't necessarily the point, but rather
that we get more natural function composition if we design our argument order to deliberately allow
for this type of partial application.
