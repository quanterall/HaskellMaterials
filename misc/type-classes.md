# Type Classes

Type classes can be found in few languages. Haskell and Scala are the primary examples of where, but
a version of them can be found in Rust's "traits".

A type class can be described as a constraint on a type variable, which is how you'll see them in
source code for the most part. The definition of a type class sets up what shape the type it can be
defined for and the instances make this type concrete, then we use it in a type signature to
constrain a type variable such that we are saying "This is generic over `a` but we are also saying
it can have this capability".

## Generics / Type variables

In order to productively talk about type classes we therefore need to show how employing them in a
type signature will change the nature of the function.

### identity

The "identity function" is a function that simply returns the argument passed to it:

```haskell
identity :: a -> a
```

It is obviously generic over `a` because anything can be passed to it and it'll be returned. The
merits of this function become clear when working with code so I won't stress how useful it is, but
I'll try to explain why this function is the backbone of type variables coupled with constraints.

What are the possible implementations of `identity`? Well, we don't have access to `IO` so we can
discount everything that has to do with effectful things. We have the value of type `a`, meaning it
can stand in for any type we throw at it. What we return has to be the same type.

Can we turn it into a string? No, that would require `identity :: a -> String`. Printing it on the
screen would require `IO`.

Long story short: The only possible (reasonable) implementation of `identity` is:

```haskell
identity :: a -> a
identity x = x
```

Doing anything else will require either adding more to the returned type (`IO`, for example) or
**constrain** the type variable `a`. We can do so as follows:

```haskell
-- The part on the left of the fat arrow is a collection of constraints.
-- Here we are adding the `Num` constraints which allows some basic arithmetic operators.
-- We can specify several constraints either with:
--   `ConstraintOne a => ConstraintTwo a => ...`
-- or:
--   `(ConstraintOne a, ConstraintTwo a) => ...`.
notIdentityAnymore :: Num a => a -> a
notIdentityAnymore x = x + 42
```

Note how **adding constraints to a type variable increases what we can do with it**. A type variable
without constraints starts out being essentially just a holder for a value of some type that is
figured out at compile-time and as we add constraints we can use it in more ways.

This dynamic allows us to add precisely the capabilities we need and want to our generic functions
and it works for container types and types that otherwise take other types.

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

We can read this as "any type that takes another type `f` and for any `a` in that type". `f` here is
"higher-kinded", because its kind is `* -> *`, meaning it takes a type in order to return a type.

There is an issue with our `containerLength` type signature, however; we are assuming all containers
have lengths. Haskell wouldn't allow any implementation of this function that didn't ignore the
container passed in and returned a number we chose, because we haven't said anything about what the
container actually supports.

To fix this, we will have to use a constraint on `f`:

```haskell
-- Note how we don't have to constrain `a` because we're not actually doing anything with it.
containerLength :: Foldable f => f a -> Int
containerLength container =
  foldr (\_item lengthSoFar -> 1 + lengthSoFar) 0 container
```

This function (and the standard library `length` that exists already) would now work for any type
that is foldable, which is a lot of types.

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
the ` | ` part means that we can define `negate` **or** `(-)` and it'll use a default version for
the other if we don't specify it.

### Eq

`Eq` gives equality comparison. When we define it we can specify the "equals" operator **or** the
"not equals" operator and the other will be filled in for us automatically.

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
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
order to define `Ord` for something it also has to have a `Eq` implementation. This is called a
"superclass" and is very common in order to automatically inherit capabilities of other constraints
when one is specified. If we specified `Ord a` in a function we would therefore be able to use all
functions associated with `Eq` as well.

### Functor

"Functor" is a big word for what is essentially "We can have a thing inside of this and modify it".
This is at least what it means in Haskell, which is what matters when we write Haskell:

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

#### The utility of this

What this means in practice is that we get the same notation for everything that is a monad. This
means that we can use `>>=`/`bind`, `pure`, `fmap`/`<$>` and friends for a whole slew of things that
implement this constraint; `STM` (software-transactional memory, pointers that have transactional
behavior much like databases), `IO`, `Async` (asynchronous IO); the list is long. We get all this
for free, as long as we understand what `Monad` expects and provides.

#### Monads and their "laws"

If something says it's a monad, it likely is, but it's useful to know that people do associate
a certain behavior with the word "monad".

There are 3 "laws" that a monad should obey; I will describe them in terms of code:

Law 1: "left identity"; `pure x` into `bind` into `f x` can be replaced with `f x`

`pure x >>= f x`  === `f x`

Law 2: "right identity"; `pure` after `bind` can be replaced with the thing before `bind`

`f x >>= pure` === `f x`

Law 3: "associativity"; it doesn't matter in which order you parenthesize `bind`

`(a >>= f) >>= g` === `a >>= (\x -> f x >>= g)`

Knowing these and thinking about them isn't something that I think most will do or care about; the
truth is that you'll internalize the behavior of monads when using them over time and this is for
the most part something that will seem obvious to you when you yourself think about this in the
future.
