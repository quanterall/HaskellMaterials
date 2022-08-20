# Optics

- [Optics](#optics)
  - [Lenses](#lenses)
    - [Getters](#getters)
    - [Setters](#setters)
    - [Lenses as getters and setters in one](#lenses-as-getters-and-setters-in-one)
      - [Exercises (Lenses as getters and setters in one)](#exercises-lenses-as-getters-and-setters-in-one)
      - [Operators](#operators)
        - [Getting/extracting values](#gettingextracting-values)
        - [& for setting values](#-for-setting-values)
          - [Setting constant values](#setting-constant-values)
          - [Modifying values](#modifying-values)
      - [Exercises (Operators)](#exercises-operators)
      - [Other notable uses of lenses](#other-notable-uses-of-lenses)
        - [`view`](#view)
      - [Lenses are not only for records](#lenses-are-not-only-for-records)
  - [Prisms](#prisms)
    - [Prisms as a way of narrowing types](#prisms-as-a-way-of-narrowing-types)
    - [Exercises (Prisms)](#exercises-prisms)
  - [Traversals and Folds](#traversals-and-folds)
    - [^.. for getting values](#-for-getting-values)
    - [& for setting values via a traversal](#-for-setting-values-via-a-traversal)
    - [Exercises (Traversals and Folds)](#exercises-traversals-and-folds)
      - [Exercise notes (Traversals and Folds)](#exercise-notes-traversals-and-folds)
  - [Optics for free](#optics-for-free)
  - [Should I use optics?](#should-i-use-optics)
  - [Learning much, much more](#learning-much-much-more)

Optics are a reasonably big part of the Haskell ecosystem and as such we'll go over what they are
and the basics of using them in order to accomplish common tasks. We won't be diving very deep into
the implementation or indeed the theoretical details of how they work, but rather how we can
internalize some of their use cases as well as how we can use them practically.

## Lenses

At the core of it, a lens comprises two things:

- A composable way to get a value from another value
- A composable way to set a value in another value

This might sound fairly vague, but we'll look at some examples of this.

### Getters

As you probably know from the earlier chapters of this material, when we define a record we get an
accessor by default for that structure that allows us to get the value from the data structure:

```haskell
data Record = Record {field :: String}

-- field :: Record -> String
```

This is essentially what a "getter" really is. We could compose this accessor with something that
previously has returned a `Record` and we would have successfully reached further inside of that and
gotten the `field` value out:

```haskell
data ThingThatStoresRecord = ThingThatStoresRecord {record :: Record}

recordField :: ThingThatStoresRecord -> String
recordField = record >>> field
```

From the above we can see that getting data inside of other data is not necessarily an issue most of
the time. However, setting data becomes more interesting.

### Setters

If we look at the above, we have no reliable way to set the value of `field` in `Record`, given a
value of type `ThingThatStoresRecord`:

```haskell
setRecordField :: String -> ThingThatStoresRecord -> ThingThatStoresRecord
setRecordField newValue ThingThatStoresRecord {record} =
  ThingThatStoresRecord {record = record {field = newValue}}
```

This kind of setter is fairly tedious and scales fairly badly in terms of nested structures. We
would like to be able to set the value of `field` in a much more ad-hoc and composable way. If
things change, we don't want to modify 3 different places where the structure of this value is used
for pattern-matching, creation or otherwise.

### Lenses as getters and setters in one

A `Lens` is defined by a pair of functions that describe how to get a thing and how to set a thing,
inside of (conceptually) a given data structure.

Let's say we start out with our example data structure, but we prefix the field with an underscore:

```haskell
data Record = Record {_field :: String}
  deriving (Eq, Show)
```

We then want to create a lens, `field`, that can get and set the value inside `Record`:

```haskell
-- `RIO` includes the basics of lenses by default
import RIO

data Record = Record {_field :: String}
  deriving (Eq, Show)

-- `Lens'` here is a version of the `Lens` type that only specifies what type we are getting/setting
-- from/into as well as the type of the value that we are getting/setting. Here we can see that we
-- are getting a `String` and setting a `String`, inside of a value of type `Record`.
field :: Lens' Record String
field = lens _field (\record newValue -> record {_field = newValue})
```

By itself, this lens isn't terribly useful, but let's take a look at what we can get out of it if we
also define a lens for the `_record` field in `ThingThatStoresRecord`:

```haskell
import RIO

data Record = Record {_field :: String}
  deriving (Eq, Show)

data ThingThatStoresRecord = ThingThatStoresRecord {_record :: Record}
  deriving (Eq, Show)

field :: Lens' Record String
field = lens _field (\record' newValue -> record' {_field = newValue})

record :: Lens' ThingThatStoresRecord Record
record = lens _record (\thing newValue -> thing {_record = newValue})
```

#### Exercises (Lenses as getters and setters in one)

1. Define lenses for a `User` structure that contains a name, an age and a list of hobbies
(`[String]`).

2. Make `_name` a `Name` type that holds the first name and last name of the user. Define lenses for
   this new type as well.

3. Create a `fullName` lens that gets and sets the full name of the user.

#### Operators

In order to get or set values in a structure, we'll have to employ a few different operators. These
operators will tell us (and the type system) what it is we want to accomplish with our lenses, since
they work for many different operations.

##### Getting/extracting values

We can see here that we are able to get values as easily via lenses as we would otherwise, if not
easier:

```haskell
Q> t = ThingThatStoresRecord {_record = Record {_field = "Hello"}}
Q> t ^. record
Record { _field = "Hello" }
Q> t ^. record . field
"Hello"
```

The `^.` operator is for reaching into a value and getting something out of it. There is a lot more
you can do with this, but for very basic usage this is all you will need for some time.

##### & for setting values

When you see a structure followed by a `&` and a lens, you should assume that we'll be modifying
some value inside of the structure. This can be done with a variety of operators, the two most
common ones being `.~` and `%~`.

###### Setting constant values

When we use `.~` to set a value, we are replacing a value with another value:

```haskell
Q> t = ThingThatStoresRecord {_record = Record {_field = "Hello"}}
Q> t & record .~ Record {_field = "This is another record"}
ThingThatStoresRecord
    { _record = Record
        { _field = "This is another record" }
    }
Q> t & record . field .~ "Hello again"
ThingThatStoresRecord
    { _record = Record
        { _field = "Hello again" }
    }
```

###### Modifying values

When we use `%~` to set a value, we are modifying a value via a function:

```haskell
Q> t = ThingThatStoresRecord {_record = Record {_field = "Hello"}}
Q> t & record . field %~ (<> " world")
ThingThatStoresRecord
    { _record = Record
        { _field = "Hello world" }
    }
```

#### Exercises (Operators)

1. Define lenses for the following structures, then compose them to see how they work together for
   getting and setting values:

```haskell
import RIO

newtype App = App
  { _appConfig :: Config
  }
  deriving (Eq, Show)

data Config = Config
  { _configUser :: User,
    _configPort :: Int
  }
  deriving (Eq, Show)
```

2. Define a function `uppercaseFirstLetterOfUsersLastName :: App -> App`. Define it in part using
   lenses.

3. If we wanted to use a lens to get the length of a `User`s full name, what could we do?

4. If we wanted to use lenses to concatenate all of the hobbies of a user into a `String`, with
   commas between each one, what could we do? Note that we are always able to use `^.` to get a
   value out of a structure and then `&` to call a function on that value.

#### Other notable uses of lenses

##### `view`

The `view` function is a way to use a lens in a `MonadReader` context:

```haskell
import Control.Lens.TH (makeLenses)
import RIO

data Record = Record {_field :: String}
  deriving (Eq, Show)

data ThingThatStoresRecord = ThingThatStoresRecord {_record :: Record}
  deriving (Eq, Show)

class HasRecordField env where
  fieldL :: Lens' env String

instance HasRecordField Record where
  fieldL = field

instance HasRecordField ThingThatStoresRecord where
  fieldL = record . field

class HasRecord env where
  recordL :: Lens' env Record

instance HasRecord Record where
  recordL = id

instance HasRecord ThingThatStoresRecord where
  recordL = record

field :: Lens' Record String
field = lens _field (\record' newValue -> record' {_field = newValue})

record :: Lens' ThingThatStoresRecord Record
record = lens _record (\thing newValue -> thing {_record = newValue})

functionUsingView :: (MonadReader env m, HasRecordField env) => m Int
functionUsingView = do
  value <- view fieldL
  pure $ length value
```

With [view](https://www.stackage.org/haddock/lts-18.20/rio-0.1.21.0/RIO.html#v:view) we can use a
lens in order to get a somewhat more flexible version of
[asks](https://www.stackage.org/haddock/lts-18.20/rio-0.1.21.0/RIO-Prelude.html#v:asks).

With [preview](https://www.stackage.org/haddock/lts-18.20/rio-0.1.21.0/RIO.html#v:preview) we can
get a version that may or may not return a value, i.e. it will return a `Maybe` of whatever we have
focused on.

#### Lenses are not only for records

Although we can use lenses to get and set values in a record, they're not limited to records at all.
We can use them to get and set values in any data structure, as long as it either already has lenses
defined for it or we are able to define them ourselves:

```haskell
-- Setting the third slot of a 3-tuple to 5
Q> (1, 2, 3) & _3 .~ 5
(1, 2, 5)
-- Adding 5 to the fourth element in a list 
Q> [1..9] & ix 3 %~ (+ 5)
[1, 2, 3, 9, 5, 6, 7, 8, 9]
-- Setting the head of a list to 42, if it exists
Q> [1..9] & _head .~ 42
[42, 2, 3, 4, 5, 6, 7, 8, 9]
Q> [] & _head .~ 42
[]
-- Setting the last element of a list to 42, if it exists
Q> [1..9] & _last .~ 42
[1, 2, 3, 4, 5, 6, 7, 8, 42]
Q> [] & _last .~ 42
[]
-- Modifying the tail of a list by adding 5 to each element, if the tail exists. `mapped` here is a
-- function that says to take the `_tail` result and apply something to each thing
Q> [1..9] & _tail . mapped %~ (+ 5)
[1, 7, 8, 9, 10, 11, 12, 13, 14]
Q> [] & _tail . mapped %~ (+ 5)
[]
```

## Prisms

In concert with lenses, we can also use prisms. A prism can be considered a function that will
answer a question about the target of a lens. If the target is a valid value, it will return a
`Just` value, otherwise it will return a `Nothing`.

Let's create a prism to answer whether or not a number is positive:

```haskell
positive :: (Num a, Ord a) => Prism' a a
positive = prism' id (\x -> if x > 0 then Just x else Nothing)
```

With this in hand we can do the following:

```haskell
Q> [-5..5] & mapped . positive .~ 42
[-5, -4, -3, -2, -1, 0, 42, 42, 42, 42, 42]
```

We can also take this further and create a prism that says a thing satisfies a predicate:

```haskell
satisfying :: (a -> Bool) -> Prism' a a
satisfying p = prism' id (\x -> if p x then Just x else Nothing)

positive :: (Num a, Ord a) => Prism' a a
positive = satisfying (> 0)

even' :: (Integral a) => Prism' a a
even' = satisfying even
```

```haskell
Q> [-5..5] & mapped . positive . even' .~ 42
[-5, -4, -3, -2, -1, 0, 1, 42, 3, 42, 5]
Q> [-5..5] & mapped . satisfying (> 0) . satisfying even .~ 42
[-5, -4, -3, -2, -1, 0, 1, 42, 3, 42, 5]
```

There are also common prisms that have to do with whether or not something has a certain structure:

- `_Left`: If the target is a `Left` value, it will return `Just` the value inside of the `Left`,
  otherwise `Nothing`
- `_Right`: If the target is a `Right` value, it will return `Just` the value inside of the `Right`
  otherwise `Nothing`
- `_Just`: If the target is a `Just` value, it will return `Just` the value inside of the `Just`
  otherwise `Nothing`

These can be very useful for ensuring that we are only applying functions in the case of `Just`,
`Left` or `Right` values being present.

### Prisms as a way of narrowing types

We can see in the examples above that we can use prisms to zoom into a value that is part of a
bigger type. In some ways we can view this as a way of further narrowing a type. We saw this also
with our `positive` prism. In fact, if we add a type to this, we can make this more obvious:

```haskell
newtype PositiveNumber a = PositiveNumber {_unPositiveNumber :: a}
  deriving (Eq, Show, Num)

_PositiveNumber :: (Ord a, Num a) => Prism' a (PositiveNumber a)
_PositiveNumber =
  prism' _unPositiveNumber (\a -> if a > 0 then Just (PositiveNumber a) else Nothing)
```

Here `PositiveNumber` represents something that is a positive number, and we can use this to narrow
down our normally just numeric types into positive versions. If we had a suite of tools that worked
only with these, we could trivially use this lens to refer only to the positive ones.

This is not just limited to these kinds of prisms, however. It's even clearer that we can refer to
a narrower type if we look at the following:

```haskell
data Event
  = FlashEvent !FlashMessageEvent
  | CurrentQueueAttributes !(Maybe QueueAttributes)
  deriving (Eq, Show)

_FlashEvent :: Prism' Event FlashMessageEvent
_FlashEvent =
  prism'
    FlashEvent
    -- This is known as a "LambdaCase", it's a way of using `case` on an incoming argument without
    -- having to bind it.
    ( \case
        FlashEvent e -> Just e
        _ -> Nothing
    )
```

In the code above we are defining this prism as a narrowing of our `Event` type. If we apply this
prism to an `Event` value we'll be able to say whether or not it's a `FlashMessageEvent` or
something else, just like with our previous prisms.

Of course, we can also go the opposite direction and use this to construct an `Event` from a
`FlashMessageEvent`. This will always work if we consider a prism a narrowing of a type, because
we'll always be able to widen a type in this scenario:

```haskell
-- `review` lets us take a prism and a value and return the "widened" type
Q> review _FlashEvent $ RemoveFlashMessage 42
FlashEvent (RemoveFlashMessage 42)
-- `re` does the same thing in a getter context
Q> RemoveFlashMessage 42 ^. re _FlashEvent
FlashEvent (RemoveFlashMessage 42)
-- `#` is an operator version of `review`
Q> _FlashEvent # RemoveFlashMessage 42
FlashEvent (RemoveFlashMessage 42)
```

This relationship between a bigger type and its sub-parts can be leveraged in areas that we've seen
before in the material but haven't been able to necessarily deal with in a concise way:

```haskell
readJsonFile :: (MonadUnliftIO m, MonadThrow m) => FilePath -> m (Maybe Value)
readJsonFile path =
  catchJust
    (^? errorTypeL . _NoSuchThing)
    (withJsonFile path (Just >>> pure))
    (const $ pure Nothing)
```

In the above code snippet we used
[catchJust](https://hackage.haskell.org/package/unliftio-0.2.22.0/docs/UnliftIO-Exception.html#v:catchJust)
to catch an exception for which our first argument (a predicate) returns `Just` a value. The
returned value will be used as an argument for the exception handling function, which is our last
argument. The `withJsonFile ...` part here is the action we want to run and potentially catch an
exception in.

`errorTypeL :: Lens' IOException IOErrorType` is a lens that lets us zoom into the type of our
exception, and we use `_NoSuchThing :: Prism' IOErrorType ()` to determine whether we have a
`NoSuchThing` exception.

### Exercises (Prisms)

1. Create a prism `_oneOf :: (Eq a) => [a] -> Prism' a a` that answer the question whether an
   element is one of the candidates passed to `_oneOf`.

2. Implement a version of `_oneOf` that uses `satisfying` instead of `prism'`.

3. Create prisms for the `RelationshipStatus` structure from chapter 1:

```haskell
import Data.Time (Day)
import Prelude

data RelationshipStatus
  = MarriedTo MarriageInfo -- This could also be `MarriedTo String Day`
  | EngagedTo UserProfile
  | ItsComplicated
  | Single
  deriving (Eq, Show)

data MarriageInfo = MarriageInfo {spouse :: String, date :: Day}
  deriving (Eq, Show)

data UserProfile = UserProfile
  { username :: String,
    age :: Int,
    active :: Bool,
    interests :: [String],
    relationshipStatus :: RelationshipStatus
  }
  deriving (Eq, Show)
```

4. Create a function using lenses and prisms that takes a `UserProfile` and returns a
   `Maybe UserProfile` where it effectively answers the question whether a user is married to
   someone who has a userprofile on our page.

## Traversals and Folds

Traversals allow us to modify multiple values in a structure at once, given some pattern that
targets those values. We saw in the previous section that we could use `_tail` to modify the tail of
a list: This is a traversal. A fold is a traversal that only works as a getter of values.

### ^.. for getting values

We can use `^..` to get a list of values from a structure if we give it a `Fold` or `Traversal`:

```haskell
Q> [1..9] ^. _tail
[2, 3, 4, 5, 6, 7, 8, 9]
Q> [1..9] ^.. _tail . traverse
[2, 3, 4, 5, 6, 7, 8, 9]
Q> [1..9] ^.. _tail . traverse . even'
[2, 4, 6, 8]
Q> "{\"a\": 4, \"b\": [{\"value\": 5}, {\"value\": 42}]}" ^.. key "b" . values . key "value"
[Number 5.0,Number 42.0]
```

### & for setting values via a traversal

As we saw in the previous section, we can set values in structures via traversals:

```haskell
Q> [1..9] & _tail . traverse . even' .~ 42
[1, 42, 3, 42, 5, 42, 7, 42, 9]
Q> [1..9] & _tail . traverse . even' %~ (+ 1)
[1, 3, 3, 5, 5, 7, 7, 9, 9]
Q> [1..9] & _tail . traverse . even' %~ (* 2)
[1, 4, 3, 8, 5, 12, 7, 16, 9]
Q> "{\"a\": 4, \"b\": [{\"v\": 5}, {\"v\": 42}]}" & key "b" . values . key "v" . _Number %~ (+ 2)
"{\"a\":4,\"b\":[{\"v\":7},{\"v\":44}]}"
```

### Exercises (Traversals and Folds)

1. Use a traversal to create a function that will traverse the values of a map and uppercase all the
   keys.

2. Use a traversal to create a function that will traverse over a `Value`[0] and if it's an object,
   will modify any values that are strings to be `Number` if they can be read as numbers.

#### Exercise notes (Traversals and Folds)

0. [aeson-lens](https://hackage.haskell.org/package/aeson-lens-0.5.0.0/docs/Data-Aeson-Lens.html)

## Optics for free

If you want to have lenses defined for your data structures without creating them yourself, you can
do the following:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module MyModule where

import Control.Lens.TH (makeLenses)
import RIO

data Record = Record {_field :: String}

makeLenses ''Record

data ThingThatStoresRecord = ThingThatStoresRecord {_record :: Record}

makeLenses ''ThingThatStoresRecord
```

With these `makeLenses` calls we get lenses that work exactly the way we defined them before, but we
don't have to define them ourselves. Note that this uses "Template Haskell" to generate the lenses
and so it's important to keep its quirks in mind. We could, for example, not put the
`ThingThatStoresRecord` definition and call to `makeLenses` before the `Record` definition, because
Template Haskell splits modules up into separate compilation phases, effectively. Because of this,
the order of definitions matters if we use the `makeLenses` calls like we did above.

We can combat this quirk somewhat by using `makeLenses` together with `foldMapM`:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module MyModule where

import Control.Lens.TH (makeLenses)
import RIO

data ThingThatStoresRecord = ThingThatStoresRecord {_record :: Record}

data Record = Record {_field :: String}

foldMapM makeLenses [''ThingThatStoresRecord, ''Record]
```

With the above code there is only one template haskell call and so the phase restriction does not
come into play in the same way.

If we want prisms to be defined we can use `makeClassyPrisms` as well:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module MyModule where

import Control.Lens.TH (makeClassyPrisms)
import RIO

data Event
  = FlashEvent !FlashMessageEvent
  | CurrentQueueAttributes !(Maybe QueueAttributes)
  deriving (Eq, Show)

makeClassyPrisms ''Event
```

This automatically defines a class called `AsEvent`:

```haskell
class AsEvent a where
  _Event :: Prism' a Event
  _FlashEvent :: Prism' a FlashMessageEvent
  _CurrentQueueAttributes :: Prism' a (Maybe QueueAttributes)
  _FlashEvent = ((.) _Event) _FlashEvent
  _CurrentQueueAttributes = ((.) _Event) _CurrentQueueAttributes

instance AsEvent Event where
  _Event = id
  _FlashEvent =
    prism
      FlashEvent
      (\case
         FlashEvent flashMessageEvent -> Right flashMessageEvent
         e -> Left e)
  _CurrentQueueAttributes =
    prism
      CurrentQueueAttributes
      (\case
         CurrentQueueAttributes maybeQueueAttributes -> Right maybeQueueAttributes
         e -> Left e)
```

In terms of the class, we see that in addition to the constructor prisms we also have one for the
`Event` type. We also get implementations for this where the prism definition for turning an `Event`
into an `Event` is predictably `id` and the other definitions, while cleaned up somewhat manually
here, are defined as illustrated above.

## Should I use optics?

It's a good idea to start using lenses in a limited way in order to internalize what kind of code we
end up with by using them. There are libraries, like
[brick](https://hackage.haskell.org/package/brick) that put lenses front and center for much of its
functionality, even expecting them to be defined for structures interacting with library functions.
The very popular [amazonka](https://hackage.haskell.org/package/amazonka) library and its
sub-libraries also use lenses for creation of the different commands one can execute in order to do
things with AWS.

You will most certainly make things easier for yourself once you start using lenses, even in a very
limited fashion. Having access to a composable way of viewing and modifying data structures is
guaranteed to be a positive, but it does require some up-front and practical learning.

## Learning much, much more

If you want to learn more about lenses, you can find a lot of information in the documentation for
the [lens package](https://hackage.haskell.org/package/lens). The
[lens wiki](https://github.com/ekmett/lens/wiki) also has a fair amount of information.
