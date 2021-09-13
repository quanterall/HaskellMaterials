# Type Classes

- [Type Classes](#type-classes)
  - [Generics / Type variables](#generics--type-variables)
    - [identity](#identity)
    - [Constraints are **transitive**](#constraints-are-transitive)
  - [Container types & type variables](#container-types--type-variables)
  - [Higher-kinded types](#higher-kinded-types)
  - [Important and common type classes](#important-and-common-type-classes)
    - [Num](#num)
      - [Exercises (Num)](#exercises-num)
    - [Eq](#eq)
      - [Exercises (Eq)](#exercises-eq)
    - [Ord](#ord)
      - [Exercises (Ord)](#exercises-ord)
    - [Semigroup](#semigroup)
    - [Monoid](#monoid)
    - [Functor](#functor)
    - [Applicative](#applicative)
    - [Monad](#monad)
      - [`do`-notation](#do-notation)
      - [Monads and their "laws"](#monads-and-their-laws)

Type classes can be found in few languages. Haskell and Scala are the primary examples of where, but
a version of them can be found in Rust's "traits". C++20 also has the "concepts" feature, which
ultimately is (or was supposed to be?) an extended (and seemingly much more powerful) version of
type classes / traits.

A type class defines a set of functions that one can implement for a type. It enables us to write
functions that rely on some commonly shared behavior and then using those functions for many
different types, which all have their own implementations of the shared behavior.

A type class can be described as a constraint on a type variable, which is how you'll see them in
source code for the most part. The definition of a type class is a definition of what capabilities
the type should have in order to be an instance of that type class. What that means in a practical
sense is that it should have definitions for a set of functions determined in the declaration of the
type class itself. These have to have the same types as the declarations in the type class:

```haskell
-- This `a` here is a type variable representing any type; the type the instance applies to.
class Hashable a where
  hash :: a -> HashData

-- Note how we are saying `a` is `String` in this instance.
instance Hashable String where 
  hash = hashString -- <- Implemented elsewhere, produces `HashData`

data FileData = FileData
  { fileName :: String,
    fileHash :: HashData
  }

createFileData :: (Hashable a) => String -> a -> FileData
createFileData fileName value = FileData {fileName, fileHash = hash value}
```

In the above example we define a type class for what it means to be able to hash something, then an
instance of this class for the type `String`. We then have a function that requires the generic type
that is passed to it to be hashable, which means we could pass `String` to it, but nothing else (
unless we implemented `Hashable` for those types). The only reason we can use the `hash` function in
`createFileData` is because we've said that the generic type passed in is `Hashable`. This makes it
so that we have to effectively specify our behavior requirements on these generic values in the type
signature.

When we use a type class in a type signature to constrain a type variable `a` we are saying "This is
generic over any type `a` that has an instance of this type class".

## Generics / Type variables

In order to productively talk about type classes we ought to show how employing them in a type
signature will change the nature of the function, so let's take a few steps back to the simplest
generic function you can implement and how it works in Haskell.

### identity

The "identity function" is a function that simply returns the argument passed to it:

```haskell
identity :: a -> a
```

It is generic over `a` because anything can be passed to it and it'll be returned. The merits of
this function become clear when working with code so I won't stress how useful it is, but I'll try
to explain why this function is the backbone of type variables coupled with constraints.

What are the possible implementations of `identity`? Well, we don't have access to `IO` so we can
discount everything that has to do with effectful things. We have the value of type `a`, meaning it
can stand in for any type we throw at it. What we return has to be the same type.

Can we turn it into a string? No, that would require `identity :: a -> String`.

Long story short: The only possible (reasonable) implementation of `identity` is:

```haskell
identity :: a -> a
identity x = x
```

Doing anything else will require either adding more to the returned type (`IO`, for example) or
**constrain** the type variable `a`. We can do so as follows:

```haskell
import Prelude

-- The part on the left of the fat arrow is a collection of constraints.
-- Here we are adding the `Num` constraint which allows some basic arithmetic operators.
-- We can specify several constraints either with:
--   `ConstraintOne a => ConstraintTwo a => ...`
-- or:
--   `(ConstraintOne a, ConstraintTwo a) => ...`.
notIdentityAnymore :: (Num a) => a -> a
notIdentityAnymore x = x + 42
```

It's notable that constraining the input type with type class constraints makes the implementation
space larger as you add more, as we can see by adding this `Eq` constraint to our previous function:

```haskell
import Prelude

notIdentityAnymore :: (Eq a, Num a) => a -> a
notIdentityAnymore x =
  if x == 0
    then 42
    else x + 1337
```

We're suddenly able to compare the input for equality with certain numbers and thus the implementation
space has grown considerably in possible complexity.

The point here is not to dissuade anyone from adding type class constraints, of course, because that
would be counter-productive. The point is rather to highlight how generic arguments/type variables
with constraints create the sense of adding power to a function and/or container, and that following
the principle of least power is both useful and idiomatic.

### Constraints are **transitive**

If we were to use `notIdentityAnymore` on a value with a generic type in another function, that type
signature would take on the same constraints as `notIdentityAnymore` as well as the ones it already
had:

```haskell
import Data.Monoid
import Prelude

notIdentityAnymore :: (Eq a, Num a) => a -> a
notIdentityAnymore x =
  if x == 0
    then 42
    else x + 1337

upperLevelFunction :: (Monoid a) => a -> a -> a
upperLevelFunction x y = x <> y

-- This would not compile without `Eq a` & `Num a` being specified
upperLevelFunction' :: (Monoid a, Eq a, Num a) => a -> a -> a
upperLevelFunction' x y = notIdentityAnymore x <> y

main :: IO ()
main = do
  print $ upperLevelFunction (Sum 41) $ Sum 1 -- Sum 42
  print $ upperLevelFunction' (Sum 41) $ Sum 1 -- Sum 1379
```

## Container types & type variables

Let's look at some container type signatures to get a sense of how Haskell handles container types
in type signatures.

```haskell
-- The Haskell "list" type is `[]` and has special treatment in the language.
listLength :: [a] -> Int

-- Written like other container types in Haskell we could write either of the following.
listLength :: [] a -> Int

-- This one isn't available in practice but in concept this is actually what `[a]` expresses.
listLength' :: List a -> Int
```

We can see that the container type precedes the type argument `a` and what we get is a "list of As".

If you were to generalize this you would then get:

```haskell
containerLength :: f a -> Int
```

We can read this as "any type `f` that takes another type `a`". `f` here is "higher-kinded", because
its kind is `* -> *`, meaning it takes a type in order to return a type. Haskell understands that we
are referring to a type that takes another type implicitly, which is part of what sets it apart from
other languages with generics. We are unable to talk about generic container types in OCaml, Rust
and indeed most other languages. Scala has support for higher-kinded polymorphism (talking
generically about these higher-kinded types, types that take other types) and there are a few less
well-known languages that do as well.

So we can now talk about these higher-kinded types in a generic fashion; `containerLength` applies
to any type that takes other types. There is an issue with our type signature, however; we are
assuming all passed in types `f` have lengths. Haskell wouldn't allow any implementation of this
function that didn't ignore the container passed in and returned a number we chose, because we
haven't said anything about what behaviors the type `f` supports/has.

To fix this, we will have to use a constraint on `f`:

```haskell
-- Note how we don't have to constrain `a` because we're not actually doing anything with it.
containerLength :: (Foldable f) => f a -> Int
containerLength container =
  foldr (\_item lengthSoFar -> 1 + lengthSoFar) 0 container
```

This function (and the standard library `length` that exists already) would now work for any type
that is foldable, which is a lot of types, a couple of them illustrated here:

```haskell
import qualified Data.Set as Set
import qualified Data.Map as Map
import Prelude

containerLength :: (Foldable f) => f a -> Int
containerLength container =
  foldr (\_item lengthSoFar -> 1 + lengthSoFar) 0 container

main :: IO ()
main = do
  print $ containerLength [1, 2, 3, 4] -- `[Int]`, length 4
  print $ containerLength (Just "one") -- `Maybe String`, length 1
  print $ containerLength $ Set.fromList [1, 1, 2, 3, 4] -- `Set Int`, length 4, note the duplicate
  let exampleMap =
        Map.fromList [("one", 1), ("two", 2), ("three", 3), ("four", 4)]
  print $ containerLength exampleMap -- `Map String Int`, length 4
```

The `containerLength` function is written with the help of `foldr`, which is a member of the
`Foldable` class. We are not using a general, "work-for-all-types" function, but one that someone
has to implement for a type that they want to make `Foldable`. While this leads to work done
specifically to make a type `Foldable`, it also means that we get an implementation that works
especially for the type in question. It also means that during implementation it (usually) becomes
very clear whether or not the concept will even work as we implement the needed functionality,
though this depends on how well defined the type class is and whether or not its shape fits well
into the type system.

## Higher-kinded types

It can be helpful to draw a parallell to "higher-order functions", i.e. functions that take and/or
return other functions. Types that take type arguments can be seen as "higher-order types" that take
type arguments in order to return concrete types.

In reality, all types have kinds in Haskell, as we can observe in `ghci`:

```haskell
Q> :kind Int
Int :: *
Q> :kind Maybe
Maybe :: * -> *
Q> :kind Either
Either :: * -> * -> *
Q> :kind []
[] :: * -> *
Q> :kind IO
IO :: * -> *
Q> :kind Map
Map :: * -> * -> *
Q> :kind Set
Set :: * -> *
```

The common thread here is that for each type, the amount of asterisks we see are directly related to
how many type arguments the types take. `Int` has zero type arguments and just referring to `Int` is
itself enough to have a concrete type.

In contrast, `IO`, `[]`, `Set` and `Maybe` take one type argument, so if we say only `[]`, `Set` and
`Maybe` we can see that they still take more arguments to create concrete types. We can still do the
following, however:

```haskell
Q> :kind IO Int
IO Int :: *
Q> :kind [Int]
[Int] :: *
Q> :kind Maybe String
Maybe String :: *
Q> :kind Set Float
Set Float :: *
```

Since we've now passed type arguments to these type constructors we're now back to one asterisk,
meaning we have concrete types. With this in mind it's not hard to see why type constructors can be
considered function applications in the type-level.

To provide a complete picture, let's see the same with `Map` and `Either`:

```haskell
Q> :kind Either
Either :: * -> * -> *
Q> :kind Either String
Either String :: * -> *
Q> :kind Either String (IO Int)
Either String (IO Int) :: *
Q> :kind Map
Map :: * -> * -> *
Q> :kind Map String
Map String :: * -> *
Q> :kind Map String Int
Map String Int :: *
```

## Important and common type classes

### Num

We saw parts of `Num`, which adds numeric operators:

```haskell
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
```

These are the operations constraining our generic types with `Num` gets us. The `MINIMAL` part means
that in order to satisfy the `Num` constraint we need to provide at least the listed functions, and
the `|` part means that we can define `negate` **or** `(-)` and it'll use a default version for
the other if we don't specify it.

#### Exercises (Num)

1. Define a function that takes a `[a]` and returns the sum of all elements. The return value type
   should be `a` as well. Otherwise, feel free to implement it any way you choose.

2. Reimplement the rectangle area function you defined in chapter 1, but for any type `a` that has
   a `Num` instance.

### Eq

`Eq` gives equality comparison. When we define it we can specify the "equals" operator **or** the
"not equals" operator and the other will be filled in for us automatically.

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
```

#### Exercises (Eq)

In order to implement these solutions you may have to add constraints to them.

1. Define a function `isInList :: a -> [a] -> Bool`. Do not use `elem`.

2. Define a function `removeElement :: a -> [a] -> RemovalResult` that finds the first occurrence of
   `a` in the list, returning both the element and the list without it if it can find it. If not, it
   signals this in the return type and returns the list as-is.

3. Define a function `allNotEqual :: a -> [a] -> [a]` that gets all the elements in the list that
   are not equal to the first parameter.

4. Define a function `mapNotEqual :: a -> (a -> b) -> [a] -> [b]` that maps a function over all
   elements in a list that are not equal to the first parameter given to the function.

5. Define an `Eq` instance manually for the following datatype:

   ```haskell
   data PotentiallyAValue a
     = DefinitelyAValue a
     | NotAtAllAValue
   ```

6. Define an `Eq` instance manually for the following datatype:

   ```haskell
   data SuccessOrFailure f s
     = Success s
     | Failure f
   ```

### Ord

`Ord` is about orderings; in short it gives us comparators like "less than", "greater than" and so
on.

```haskell
data Ordering = LT | EQ | GT

class Eq a => Ord a where
  compare :: a -> a -> Ordering -- see above for this definition
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
```

Note how there is a qualification here to the left of the `Ord a` bit: We are saying here that in
order to define `Ord` for something it also has to have an `Eq` implementation. This is called a
"superclass" and is not uncommon.

What specifying a superclass requirement for a class does is make it so that in order to define an
instance of the class the compiler will also ensure that an instance can be found for the superclass.
This makes sure that if there are any conceptual assumptions that make sense they're enforced by the
compiler to exist. For something to be ordered there has to be some notion of equality comparison,
hence the superclass requirement.

When we use an `Ord` constraint in a function type, we get access in that function to both the `Ord`
functions as well as the `Eq` functions, because the existence of an `Ord` instance also implies the
existence of `Eq`.

#### Exercises (Ord)

In order to implement these solutions you may have to add constraints to them.

1. Implement `clamp` from chapter 1 but for a type `a`. It takes a lower bound, an upper bound and
   an `a`. If `a` is lower than the lower bound, the lower bound is returned. If it's greater than
   the upper bound, we return the upper bound. Otherwise we return `a`.

2. Define a function `allBetween :: a -> a -> [a] -> [a]` that returns all `a`s in the list that are
   between the first and second argument.

### Semigroup

A semigroup is a type that has a function `<>` with which you can combine two values of that type
into a new one. A very common example of this is lists; `<>` for lists allows us to append one list
to another, giving us a new list. String types also have this property, as well as numeric types for
both addition as well as multiplication:

```haskell
"Hello " <> "Quanterall" == "Hello Quanterall"
Sum 41 <> Sum 1 == Sum 42
Product 13.37 <> Product 2 == Product 26.74
[1, 2] <> [3, 4] == [1, 2, 3, 4]
```

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
  {-# MINIMAL (<>) #-}
```

Any implementation of `Semigroup` should also satisfy the following rules:

```haskell
(a <> b) <> c == a <> (b <> c)
```

That is to say, the parenthesis here should have no effect; we should be able to append these in any
execution order and still get the same results.

### Monoid

A `Monoid` is a semigroup that also has a defined **empty value** (that we can refer to with
`mempty`) that when combined with a non-empty value will have no effect; it will return the same
value. Some examples for the semigroup examples we gave before:

```haskell
"Quanterall" <> mempty == "Quanterall"
Sum 41       <> mempty == Sum 41
Product 10   <> mempty == Product 10
[1, 2, 3, 4] <> mempty == [1, 2, 3, 4]
```

The class itself is defined as follows:

```haskell
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty #-}
```

We can see that we again have the superclass requirement (`Semigroup a => ...`) which ensures that
everything we are trying to define a `Monoid` instance of will also require a `Semigroup` instance.

For free when we define `mempty` we also get the functions `mconcat` and `mappend`. `mappend` is
really just a historical artifact and is exactly the same as `<>`, but `mconcat` allows us to take
a list of `a` and concatenate all the elements together to form an `a`:

```haskell
import Data.Monoid (Sum, Product)
-- `&` is function composition forwards through a pipeline, much like bash/Elm/Elixir
import Data.Function ((&))
import Prelude

concattedString = mconcat ["Hello", "There", "", "General", "Kenobi"] -- "HelloThereGeneralKenobi"
concattedSum = [1, 2, 3, 0, 4] & map Sum & mconcat -- Sum 10
concattedProduct = [1, 2, 3, 4] & map Product & mconcat -- Product 24
concattedList = mconcat [[1], [], [2, 3], [], [], [4]] -- [1, 2, 3, 4]
```

### Functor

"Functor" is a big word for what can in many cases be described as "We can have a thing inside of
this and modify it":

```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
```

To satisfy this constraint we only need to define `fmap`. It takes a function from `a` to `b`, a
higher-kinded type `f` that "contains" an `a` and turns that thing inside into a `b`.

The poster-child for this kind of operation is usually a list that is being "mapped" over to create
a new list, but it's important to take the type signature for what it is; it's not strictly about
containers and values inside of those containers. We can, for example, map over an `IO` action to
modify the thing that is being returned from it: `(a -> b) -> IO a -> IO b`.

You can view `fmap` (or commonly just called `map`) as a way to take a function working with `a` and
`b` and lifting it into the world of `f a` and `f b`, a different context. We can look further into
the type signature of `fmap` and see this more clearly:

```haskell
fmap :: (a -> b) -> f a -> f b
--       function    lifted function
fmap' :: (a -> b) -> (f a -> f b)
```

If we were to call just `fmap f`, we can see that the output will indeed be a function that works
for any `f` that implements `Functor`:

```haskell
import qualified System.Environment as Environment
-- `Data.Text` has a function `toUpper`: `Text -> Text`
import Data.Text (Text, toUpper, pack)
import Prelude

toUpperInF :: (Functor f) => f Text -> f Text
toUpperInF = fmap toUpper

-- | Retrieves an environment variable as text
getEnvAsText :: String -> IO Text
-- note how mapping `pack` over the result will take the returned string and convert it to `Text`
-- We could also write this as: `fmap pack $ Environment.getEnv variable`, `<$>` is just an
-- operator alias for `fmap`
getEnvAsText variable = pack <$> Environment.getEnv variable

main :: IO ()
main = do
  let justResult = toUpperInF (Just "Hello") -- Just "HELLO"
      nothingResult = toUpperInF Nothing -- Nothing
      rightResult = toUpperInF (Right "Quanterall") -- Right "QUANTERALL"
      leftResult = toUpperInF (Left "Quanterall") -- Left "Quanterall"
  ioResult <- toUpperInF $ getEnvAsText "HOME" -- "/HOME/GONZ"
  print ioResult
```

This only works because we have generalized over the concept of these different contexts and they
all support `Functor`. It gives us the capability to use our "lifted" functions with anything that
conforms to this interface.

**Note**: You will hopefully note that both `Maybe` (`Just` & `Nothing`) and `Either` (`Left` and
`Right`) have a case where `fmap` does not change the value. This is not an oversight. It is quite
instructive to try to implement a `Functor` instance for `Either` that tries to map over both cases,
in order to see why it cannot work. It also happens to be a central part of the behavior of
`Functor` (and `Monad` by extension) and works out quite well.

### Applicative

`Applicative` assumes `Functor` and in short provides us with a way to wrap a value in our type `f`
(`pure`) and take a wrapped function and apply it to a wrapped value.

```haskell
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
```

### Monad

`Monad` builds on top of `Applicative`, which means it also has the capabilities of `Functor` as
well as the capability of "unpacking" a value and applying a function that produces a new value
wrapped in the functor/monad. Note also that `return` is the same thing as `pure` and is here only
for historical reasons.

```haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=) #-}
```

#### `do`-notation

The `>>=` operator is called `bind` and is what we are implicitly using when we use `<-` in
`do`-notation:

```haskell
main = do
  environmentValue <- Environment.getEnv "VARIABLE_NAME"
  putStrLn environmentValue
```

Written using `>>=` this would instead become:

```haskell
main =
  Environment.getEnv "VARIABLE_NAME" >>=
    \environmentValue -> putStrLn environmentValue
```

It's quite common to write shorter monadic expressions in this form, though I would likely recommend
using `do`-notation as a default.

What this means in practice is that we get the same notation for everything that is a monad. This
means that we can use `>>=`/`bind`/`<-`, `pure`, `fmap`/`<$>` and friends for a whole slew of things
that implement this constraint; `STM` (software-transactional memory, pointers that have
transactional behavior much like databases), `IO`, `Async` (asynchronous IO); the list is long. We
get all this for free, as long as we understand what `Monad` expects and provides.

#### Monads and their "laws"

If something says it's a monad, it likely is, but it's useful to know that people do associate
a certain behavior with the word "monad".

There are 3 "laws" that a monad should obey; I will describe them in terms of code:

Law 1: "left identity"; `pure x` into `bind` into `f` can be replaced with `f x`

`pure x >>= f` === `f x`

Written another way:

```haskell
main = do
  value <- pure x
  f value
```

===

```haskell
main = do
  f x
```

Law 2: "right identity"; `pure` after `bind` can be replaced with the thing before `bind`

`f x >>= pure` === `f x`

Written another way:

```haskell
main = do
  value <- f x
  pure value
```

===

```haskell
main = do
  f x
```

Law 3: "associativity"; it doesn't matter how you parenthesize `bind`

`(a >>= f) >>= g` === `a >>= (\x -> f x >>= g)`

Written another way:

```haskell
m = do
  value <- a >>= f
  g value
```

===

```haskell
m = do
  value <- a
  f value >>= g
```

===

```haskell
m = a >>= f >>= g
```

Meaning, several monadic expressions in the same order should produce the same result regardless of
how they were composed together. This means that we can bundle these up however we like and still
get the same results as if we specified all of them inline.

Knowing these and thinking about them isn't something that I think most will do or care about; the
truth is that you'll internalize the behavior of monads when using them over time and this is for
the most part something that will seem obvious to you when you yourself think about this in the
future.
