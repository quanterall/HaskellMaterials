# Composite datatypes

Not everything is just a primitive, of course, and we've actually already seen an example of a more
complex datatype in the previous chapter about "Values and functions": `Bool`.

## Bool

`Bool` is actually defined exactly as we would define one of our own types:

```haskell
data Bool = True | False
```

It can be either `True` or `False` and working with it is fairly instructive in terms of how one can
work with these kinds of data declarations:

```haskell
import Prelude

add42or1337 :: Bool -> Int -> Int
-- Note that `if` is an expression and both branches need to return the same type. We also always
-- need the `else` branch for this reason.
add42or1337 shouldAdd42 x = x + if shouldAdd42 then 42 else 1337
```

We could also use pattern-matching via the `case` keyword here to inspect the value of the bool:

```haskell
import Prelude

add42or1337 :: Bool -> Int -> Int
add42or1337 shouldAdd42 x = x + case shouldAdd42 of
  True -> 42
  False -> 1337
```

Each `case` branch can deconstruct the different constructors of a union type (which `Bool` is),
even if they had associated data. We'll see this later.

We could also pattern-match "in the top-level", meaning on the left of the `=`:

```haskell
import Prelude

add42or1337 :: Bool -> Int -> Int
add42or1337 True x = x + 42
add42or1337 False x = x + 1337
```

If the logic for a function differs a lot between the cases I would personally prefer the last
version as it also allows you to have some of the arguments be bound only for certain cases, etc.,
and generally keeps each case separate. In this case the first version makes the most sense because
only the amount added depends on this boolean.

## Newtypes

Before we move into advanced composite types, let's look at a very basic tool in the Haskell
toolbelt: `newtype`.

Newtypes are types that wrap other types in order to make distinct versions of them. Let's look at
what that means in practice.

Let's imagine a function `filteredCopy` that takes a source filename, destination filename as well
as a string to filter by such that we only copy lines that contain that string:

```haskell
filteredCopy :: String -> String -> String -> IO ()
filteredCopy source destination copyPattern = ...
```

The `IO ()` for the purposes of this example means that we are doing something effectful and there
is no useful return value.

What happens if we by mistake use this function with `filteredCopy destination source copyPattern`
or any other incorrect order for the parameters? The type system doesn't know anything about what
these strings represent.

The solution is fairly simple:

```haskell
newtype Source = Source String

newtype Destination = Destination String

newtype CopyPattern = CopyPattern String

filteredCopy :: Source -> Destination -> CopyPattern -> IO ()
filteredCopy source destination copyPattern = ...
```

When we use `filteredCopy` now we will have to wrap our strings:

```haskell
filteredCopy (Source source) (Destination destination) (CopyPattern copyPattern)
```

We can still make the mistake of wrapping our `source` in a `Destination` wrapper, to be clear, but
it's much easier to spot this mistake and if a value is produced in one place in a program as a
`Destination` it simply cannot be passed blindly to a place where a `Source` is required.

## Record types

Records are useful when we want to store multiple values together in a named structure. The
individual parts, or "fields", are named as well. We begin a record definition with the keyword
`data` after which we give the name of the type. Following an `=` we then give the constructor for
the type; a function that takes the record data and constructs the type.

```haskell
--      type       constructor
data UserProfile = UserProfile
  { username :: String,
    age :: Int,
    active :: Bool,
    interests :: [String]
  }
```

The constructor name can be different than the type name, but this is comparatively rare.

One thing to note about record definitions in Haskell is that each field will have an associated
function that takes the type and returns the field:

```haskell
Q> :t username
username :: UserProfile -> String
Q> :t age
age :: UserProfile -> Int
Q> :t active
active :: UserProfile -> Bool
Q> :t interests
interests :: UserProfile -> [String]
Q> rickard = UserProfile {
     username = "rickard",
     age = 33,
     active = True,
     interests = ["Programming", "Problem Solving", "Teaching"]
   }
Q> rickard
UserProfile
    { username = "rickard"
    , age = 33
    , active = True
    , interests =
        [ "Programming"
        , "Problem Solving"
        , "Teaching"
        ]
    }
Q> username rickard
"rickard"
Q> age rickard
33
Q> active rickard
True
Q> interests rickard
[ "Programming"
, "Problem Solving"
, "Teaching"
]
```

We can see this in action in this snippet where we turn a profile into a presentation string:

```haskell
profileToString :: UserProfile -> String
profileToString profile =
  let ageString = show $ age profile
      activeString = if active profile then "active" else "not Active"
      interestsString = intercalate ", " (interests profile)
   in mconcat
        [ username profile,
          " (",
          ageString,
          "y, ",
          activeString,
          ") is interested in: ",
          interestsString
        ]

intercalate :: String -> [String] -> String
intercalate between strings =
  mconcat $ List.intersperse between strings
```

Running this on our previously defined profile we get:

```haskell
Q> profileToString rickard
"rickard (33y, active) is interested in: Programming, Problem Solving, Teaching"
```

**Note:** Record types are sometimes called product types. Later material will elaborate on why that
is and what it means.

## Union types

While a record represents a collection of values that make up a whole, all of them present, a
**union type** represents a set of alternatives that are all valid, but only one at a time. The
built-in `Bool` type is a union type; we can only have either `True` **or** `False`.

We define a union type with the `data` keyword followed by the type name and `=`. Then we list the
**constructors** of the type with `|` between them:

```haskell
data RelationshipStatus
  = MarriedTo MarriageData -- This could also be `MarriedTo UserProfile Day`
  | EngagedTo UserProfile
  | ItsComplicated
  | Single

data MarriageInfo = MarriageInfo {spouse :: String, date :: Day}
```

The different constructors all represent different cases and contain different data. In the case of
`MarriedTo` the constructor holds an entire record. If we wanted to we could take several arguments
to the constructor, but have elected to name the components because it can sometimes be clearer to
take an entire record. In the case of `EngagedTo` it's perfectly clear that the user is engaged to
another user profile. For the subsequent cases the constructors don't carry any additional data.

## Combining records and unions

As we saw in the previous section it's trivial to combine records and unions; our `MarriageInfo` type
is already embedded in the `MarriedTo` constructor. So let's take that one step further and enrich
our `UserProfile` data type:

```haskell
import qualified Data.List as List
import Data.Time (Day)
import qualified Data.Time as Time
import Prelude

data UserProfile = UserProfile
  { username :: String,
    age :: Int,
    active :: Bool,
    interests :: [String],
    relationshipStatus :: RelationshipStatus
  }
  deriving (Eq, Show)

data RelationshipStatus
  = MarriedTo MarriageInfo
  | EngagedTo UserProfile
  | ItsComplicated
  | Single
  deriving (Eq, Show)

data MarriageInfo = MarriageInfo {spouse :: String, date :: Day}
  deriving (Eq, Show)

profileToString :: UserProfile -> String
profileToString profile =
  let ageString = show $ age profile
      activeString = if active profile then "active" else "not Active"
      interestsString = intercalate ", " (interests profile)
      relationshipStatusString = case relationshipStatus profile of
        MarriedTo MarriageInfo {spouse, date} ->
          let dateString = Time.showGregorian date
           in -- `unwords` takes a `[String]` and joins them into a string with spaces inbetween
              unwords ["Married to:", spouse, "on", dateString]
        EngagedTo UserProfile {username} -> unwords ["Engaged to:", username]
        ItsComplicated -> "It's complicated"
        Single -> "Single"
   in mconcat
        [ username profile,
          " (",
          ageString,
          "y, ",
          activeString,
          ", ",
          relationshipStatusString,
          ") is interested in: ",
          interestsString
        ]

intercalate :: String -> [String] -> String
intercalate between strings =
  mconcat $ List.intersperse between strings
```

If we now construct our `rickard` profile with this in mind we get the following:

```haskell
Q> rickard = UserProfile {
     username = "rickard",
     age = 33,
     active = True,
     interests = ["Programming", "Problem Solving", "Teaching"],
     relationshipStatus = MarriedTo MarriageInfo {
       spouse = "Ivana",
       date = Time.fromGregorian 2015 06 04
     }
   }
Q> profileToString rickard
"rickard (33y, active, Married to: Ivana on 2015-06-04) is interested in: Programming, Problem Solving, Teaching"
```

Combining record types and union types is the basis for domain modelling in Haskell and allows us to
construct very rich datatypes that we can work safely with. Armed with more knowledge in the future
it will be easy for you to look at the above example and modify it according to hypothetical
business logic and having those changes be reflected in our functions.

As an example, if we decided that our marriage information should hold a `UserProfile`, that would
require users on our site to only be able to set their "married" status if their spouse is on the
site. However, if we instead make the `spouse` field take a type that allows us to have a name
**or** a userprofile, we can express this possibility clearly:

```haskell
data MarriageInfo = MarriageInfo {spouse :: Spouse, date :: Day}
  deriving (Eq, Show)

data Spouse
  = SpouseProfile UserProfile
  | SpouseName String
  deriving (Eq, Show)
```

This will give us the same capability as before, because we still support spouse names with strings:

```haskell
Q> rickard = UserProfile {
     username = "rickard",
     age = 33,
     active = True,
     interests = ["Programming", "Problem Solving", "Teaching"],
     relationshipStatus = MarriedTo MarriageInfo {
       spouse = SpouseName "Ivana",
       date = Time.fromGregorian 2015 06 04
     }
   }
Q> profileToString rickard
"rickard (33y, active, Married to: Ivana on 2015-06-04) is interested in: Programming, Problem Solving, Teaching"
```

But we can now also use a user profile in our `spouse` field:

```haskell
Q> ivana = UserProfile {username = "ivana", age = 31, active = True, interests = ["Web Design", "Cats", "Beer"], relationshipStatus = MarriedTo MarriageInfo {spouse = SpouseProfile rickard, date = Time.fromGregorian 2015 06 04}}
Q> profileToString ivana
"ivana (31y, active, Married to: rickard on 2015-06-04) is interested in: Web Design, Cats, Beer"
```

Since we are now using the `SpouseProfile` constructor for `spouse` we can pass our previously
defined `rickard` `UserProfile` and it still works. If we were building this site we could link to
to the user profile in this case but leave the other case as just a string in our presentation.

For another example of modelling (part of) a domain with types, see
[this file](./02b-person-printing.md).

## Generic datatypes

The most basic generic datatype is a type that can hold anything:

```haskell
data Holder a = Holding a
```

Note how we now have a type variable on the left side of `=` which means that when we refer to the
type it will take a type name. If we were holding a `Int`, for example, the type is `Holder Int`.
The corresponding constructor call (or pattern match) is `Holding value`.

This basic type doesn't have much going for it in terms of functionality, but it's useful to show
how we express type variables in data types. Fortunately for us we only need to extend the record
and union definitions with the same parts as we are using in this basic one in order to get generic
versions of them.

If we were to define a generic record, for example, we could just do the following:

```haskell
data HttpResponse a = HttpResponse
  { status :: HttpStatus,
    headers :: [HttpHeader],
    body :: a
  }
```

We can see here that our HTTP Response is generic over different type of body types. This means we
can construct it with different types and still get the expected structure for the rest of the
response. Since only the body will differ here we are free to say that it could be a JSON value,
bytestring or maybe UTF8 text. Much like our `Holder` example this would mean that when we refer to
the type we would have, for example, `HttpResponse JSONValue`, `HttpResponse ByteString` or
`HttpResponse UTF8Text`.

With unions we predictably have the same format for generic unions as we do for basic ones and we
will go through several popular and representative definitions below.

## Commonly used composite datatypes

It's usually very instructive to look at some of the supplied composite datatypes that one can find
in most Haskell code, so here is an introduction to their definitions and some of their use cases.

### Maybe

`Maybe a` is a type that represents the existence or non-existence of a value of type `a`. It can be
seen as a `null` type that retains its type even in the presence of nesting. It's defined as
follows:

```haskell
data Maybe a
  = Nothing
  | Just a
```

Note that we now have a type parameter on the left side of the `=` and that one of our constructors
takes this `a`.

`Maybe` is useful generally speaking wherever you would have the type `null | SomeType`, which means
that we generally would like to use it for success/failure when we don't have any interesting error
information. It's also useful for when certain options aren't supplied to a call, or information is
not available and that's fine.

You may want to replace it with what is effectively the same type but with more descriptive names,
but this should be done on a case-by-case basis.

We could for example imagine that a resource is not loaded yet in some data, and this could be
represented with a `Maybe Resource`, but we could also create the following datatype:

```haskell
data ResourceLoadStatus
  = NotYetLoaded
  | Loaded Resource
```

This is all a matter of what makes things clear. Do you want to be able to use the functions that
already exist for `Maybe`, perhaps? In that case it's reasonable to keep it as a `Maybe`. If a
custom type makes things much clearer when looking at your data structures, then go for it. It's not
a complicated endevour to define the functions you need for this type and in the case of a resource
load status it's likely the case that you're pattern matching to see if you need to load the
resource, or just want to display "N/A" when it's not loaded.

### Either

`Either l r` is a type that represents either the error case `Left` with information attached to it
or the success case `Right` with success data attached to it. The reason `Left` is always the error
case is because of how `Either` works in a monadic context. When returning `Left` it is considered
an error. It's also because `Left` is not right, meaning it's "wrong".

```haskell
data Either e a
  = Left e
  | Right a
```

`Either` is useful when we want something that can either succeed or fail but we also want to bundle
some information into our error case. It's common to return a custom error type for the `Left` case
that can be inspected to see what kind of error that was hit and any extra information that is
attached.

This, again, can be specialized down to something custom but still retain the same meaning:

```haskell
data ResourceLoadResult
  = LoadFailure ErrorData
  | LoadSuccess Resource
```

The above definition holds the same information as a `Either ErrorData Resource`, but can be more
descriptive in certain contexts.

### List / []

`List` / `[]` is interesting because it's defined in terms of operators:

```haskell
data [] a
  = []
  | a : [a]
```

So we have a type, called `[]` that takes an `a`. The constructors are `[]` itself, which is the
empty list, and `:` which as the left argument takes an `a` and as the right argument takes another
list, `[a]`.

Defined another way we have the following:

```haskell
data List a
  = EmptyList -- This is commonly called `Nil`
  | Prepend a (List a) -- This is commonly called `Cons`
```

Lists are useful any time you need to have zero or more of something. It's important to note that
while Haskell lists are technically pointers to pointers to pointers, etc. a lot of this
inefficiency can be mitigated by the fact that Haskell doesn't ask for the next value in a list
unless it needs it. This has to do with lazyness/non-strictness and is a language feature too
big to detail here.

### Tuples

A tuple is an ad-hoc collection of values that can be of different types. Tuples are a staple of
many so called functional languages and Haskell is no exception:

```haskell
tuple :: (Int, String)
tuple = (42, "Forty-Two")

tuple' :: (Int, String, Bool)
tuple' = (42, "Forty-Two", False)

tuple'' :: (Int, String, Bool, Float)
tuple'' = (42, "Forty-Two", False, 1337.0)
```

In many ways a tuple is the same as a record, except we are not naming the different components or
the constructor. Consequently it's useful when names for the components or the constructor aren't
needed because the scope of the created value is small or the names themselves would not be deemed
useful. The utility of tuples should be examined on a case-by-case basis to ensure that they don't
make code harder to understand because of their lack of information/context. A name for both a
constructor and the individual fields/components can in many cases be very illuminating.
