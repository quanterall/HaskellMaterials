# Optics

- [Optics](#optics)
  - [Lenses](#lenses)
    - [Getters](#getters)
    - [Setters](#setters)
    - [Lenses as getters and setters in one](#lenses-as-getters-and-setters-in-one)
      - [Operators](#operators)
        - [^. for getting values](#-for-getting-values)
        - [& for setting values](#-for-setting-values)
          - [Setting constant values (`.~`)](#setting-constant-values-~)
          - [Modifing values (`%~`)](#modifing-values-~)
      - [Lenses are not only for records](#lenses-are-not-only-for-records)
  - [Prisms](#prisms)
  - [Lenses for free](#lenses-for-free)
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

By itself, this lens isn't terribly useful, but let's take a look at what we can get out of it:

```haskell
import RIO

data Record = Record {_field :: String}
  deriving (Eq, Show)

data ThingThatStoresRecord = ThingThatStoresRecord {_record :: Record}
  deriving (Eq, Show)

-- `Lens'` here is a version of the `Lens` type that only specifies what type we are getting/setting
-- from/into as well as the type of the value that we are getting/setting. Here we can see that we
-- are getting a `String` and setting a `String`, inside of a value of type `Record`.
field :: Lens' Record String
field = lens _field (\record' newValue -> record' {_field = newValue})
```

#### Operators

##### ^. for getting values

We can see here that we are able to get values as easily via lenses as we would otherwise, if not
easier:

```haskell
Q> t = ThingThatStoresRecord {_record = Record {_field = "Hello"}}
Q> t ^. record
Record
    { _field = "Hello" }
Q> t ^. record . field
"Hello"
```

The `^.` operator is for reaching into a value and getting something out of it. There is a lot more
you can do with this, but for very basic usage this is all you will need for some time.

##### & for setting values

When you see a structure followed by a `&` and a lens, you should assume that we'll be modifying
some value inside of the structure. This can be done with a variety of operators, the two most
common ones being `.~` and `%~`.

###### Setting constant values (`.~`)

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

###### Modifing values (`%~`)

When we use `%~` to set a value, we are modifying a value via a function:

```haskell
Q> t = ThingThatStoresRecord {_record = Record {_field = "Hello"}}
Q> t & record . field %~ (<> " world")
ThingThatStoresRecord
    { _record = Record
        { _field = "Hello world" }
    }
```

#### Lenses are not only for records

Although we can use lenses to get and set values in a record, they're not limited to records at all.
We can use them to get and set values in any data structure, as long as it either already has lenses
defined for it or we are able to define them ourselves:

```haskell
-- Setting the third slot of a 3-tuple to 5
Q> (1, 2, 3) & _3 .~ 5
( 1, 2, 5)
-- Adding 5 to the fourth element in a list 
Q> [1..9] & ix 3 %~ (+ 5)
[1, 2, 3, 9, 5, 6, 7, 8, 9]
-- Setting the head of a list to 42, if it exists
Q> [1..9] & _head .~ 42
[42, 2, 3, 4, 5, 6, 7, 8, 9]
Q> [] & _head .~ 0
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
Q> [] & _tail %~ (+ 5)
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

These can be very useful for ensuring that we are only applying function in the case of `Just`,
`Left` or `Right` values being present.

## Lenses for free

If you want to have lenses defined for your data structures without creating them yourself, you can
do the following:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module MyModule where

import Control.Lens.TH
import RIO

data Record = Record {_field :: String}

makeLenses ''Record

data ThingThatStoresRecord = ThingThatStoresRecord {_record :: Record}

makeLenses ''ThingThatStoresRecord
-- or `foldMapM makeLenses [''ThingThatStoresRecord, ''Record]` instead of separate calls
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

## Learning much, much more

If you want to learn more about lenses, you can find a lot of information in the documentation for
the [lens package](https://hackage.haskell.org/package/lens). The
[lens wiki](https://github.com/ekmett/lens/wiki) also has a fair amount of information.
