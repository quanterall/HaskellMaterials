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
add42or1337 :: Bool -> Int -> Int
-- Note that `if` is an expression and both branches need to return the same type. We also always
-- need the `else` branch for this reason.
add42or1337 shouldAdd42 x = x + if shouldAdd42 then 42 else 1337
```

We could also use pattern-matching via the `case` keyword here to inspect the value of the bool:

```haskell
add42or1337 :: Bool -> Int -> Int
add42or1337 shouldAdd42 x = case shouldAdd42 of
  True -> x + 42
  False -> x + 1337
```

Each `case` branch can deconstruct the different constructors of a union type (which `Bool` is),
even if they had associated data. We'll see this later.

We could also pattern-match "in the top-level", meaning on the left of the `=`:

```haskell
add42or1337 :: Bool -> Int -> Int
add42or1337 True x = x + 42
add42or1337 False x = x + 1337
```

If the logic for a function differs a lot between the cases I would personally prefer the last
version as it also allows you to have some of the arguments be bound only for certain cases, etc.,
and generally keeps each case separate. In this case the first version makes the most sense because
only the amount added depends on this boolean.

## Record types

A record type is a collection of fields and values that all exist together at the same time:

```haskell
data Person = Person
  { personName :: String,
    personAge :: Age,
    personProfession :: Profession
  }
```

We only have one constructor here `Person`, and it takes a name, age and profession. The reason that
we might name fields with what they relate to is that every record field actually is a function that
takes the constructed type and returns the field's type.

```haskell
-- The constructor is actually a function that takes the arguments in order
Person :: String -> Age -> Profession -> Person

-- Each field has an associated function that extracts that thing from a `Person`
personName :: Person -> String
personAge :: Person -> Age
personProfession :: Person -> Profession
```

When we create a `Person` and want to create it like a record we can do so as follows:

```haskell
aPerson :: Person
aPerson =
  Person
    { personName = "Victor Vega",
      personAge = Living 42,
      personProfession = FictionalCharacter (Antagonist (FictionalWorkName "Reservoir Dogs"))
    }
```

It's likely best to prefer using the field names like this in order to be clearer about which fields
have which values. The alternative way is less clear about which part is what and can be very
confusing when you have big records:

```haskell
aPerson :: Person
aPerson =
  Person
    "Victor Vega"
    (Living 42)
    (FictionalCharacter (Antagonist (FictionalWorkName "Reservoir Dogs")))

```

This becomes more and more clear when you have descriptive types like the `Profession` type that
make it clearer what is what, but we should try to mention the field names when it doesn't hamper
our ability to write clear code.

## Union types

Union types are where we can see the basics of modelling broader relationships between things.

_Note that the following is a demonstration of using union types, not a logical definition of which
professions exist and what you can be a student/professor of._

```haskell
-- These are here only to distinguish "normal strings" from these particular types of strings.
-- `newtype` doesn't actually create a new type for runtime, but exists only as a distinction
-- between the types for compile-time.
newtype CompanyName = CompanyName String
newtype FictionalWorkName = FictionalWorkName String

data Age
  = Dead
  | Living Int

-- Our union cases can care about wildly different things.
data Profession
  = Programmer ProgrammerRole CompanyName
  | Professor Subject
  | Student Subject
  | FictionalCharacter FictionalCharacter
  | Unemployed

data ProgrammerRole
  = Backend
  | Frontend
  | Fullstack

data Subject
  = Finance
  | Physics AppliedOrTheoretical
  | Mathematics AppliedOrTheoretical
  | ComputerScience
  | ComputerEngineering

data AppliedOrTheoretical = Applied | Theoretical

data FictionalCharacter
  = Protagonist FictionalWorkName
  | Antagonist FictionalWorkName
```

Our types can embed other types that we've created and generally we are free to specify exactly
as much information as we care to for each case, which leads to a very organic type structure that
enables us to be very precise about what we care about in each case.

```haskell
printPerson :: Person -> String
printPerson
  Person
    { personName = name,
      personAge = Living age,
      personProfession = profession
    } =
    name <> " is a " <> show age <> " years old " <> printProfession profession
printPerson
  Person
    { personName = name,
      personAge = Dead,
      personProfession = profession
    } =
    name <> " was a " <> printProfession profession
```

Above we can see the first case of where the distinction between two cases matters: `Living` &
`Dead`. In the case where someone is living there is an age (`Int`) attached to this information, so
we pull it out and use it. We also print "is a" in this case. When a person is defined as `Dead` we
have no extra information and instead print "was a".

The rest of the printing follows much the same structure; we match on the constructors in the
top-level and decide what to print based on it. In certain cases we have access to more or less
information connected to the constructor in question and we might call another function to print
that data.

```haskell
printPerson :: Person -> String
printPerson
  Person
    { personName = name,
      personAge = Living age,
      personProfession = profession
    } =
    name <> " is a " <> show age <> " years old " <> printProfession profession
printPerson
  Person
    { personName = name,
      personAge = Dead,
      personProfession = profession
    } =
    name <> " was a " <> printProfession profession

printProfession :: Profession -> String
printProfession Unemployed = "unemployed person"
printProfession (Professor subject) = "professor of " <> printSubject subject
printProfession (Student subject) = "student of " <> printSubject subject
printProfession (FictionalCharacter fictionRole) = printFictionRole fictionRole
printProfession (Programmer role (CompanyName name)) =
  printProgrammerRole role <> " programmer at " <> name

printSubject :: Subject -> String
-- Note here how we are matching on two different physics & mathematics cases, one for applied
-- and one for theoretical.
printSubject (Physics Applied) = "applied physics"
printSubject (Physics Theoretical) = "theoretical physics"
printSubject (Mathematics Applied) = "applied mathematics"
printSubject (Mathematics Theoretical) = "theoretical mathematics"
printSubject ComputerScience = "computer science"
printSubject ComputerEngineering = "computer engineering"
printSubject Finance = "some finance thing"

printFictionRole :: FictionalCharacter -> String
printFictionRole (Protagonist (FictionalWorkName name)) =
  "protagonist of '" <> name <> "'"
printFictionRole (Antagonist (FictionalWorkName name)) =
  "antagonist of '" <> name <> "'"

printProgrammerRole :: ProgrammerRole -> String
printProgrammerRole Backend = "backend"
printProgrammerRole Frontend = "frontend"
printProgrammerRole Fullstack = "fullstack"
```

When we create these types we just follow the different constructor paths as you would expect and
the result generally is a fairly self-explanatory structure:

```haskell
aBackendProgrammer :: Person
aBackendProgrammer =
  Person
    { personName = "Kristina",
      personAge = Living 24,
      personProfession =
        Programmer Backend (CompanyName "Quanterall")
    }

aFrontendProgrammer :: Person
aFrontendProgrammer =
  Person
    { personName = "Pesho",
      personAge = Living 42,
      personProfession =
        Programmer Frontend (CompanyName "Quanterall")
    }

aFullstackProgrammer :: Person
aFullstackProgrammer =
  Person
    { personName = "Sasho",
      personAge = Living 18,
      personProfession =
        Programmer Fullstack (CompanyName "Quanterall")
    }

aProfessor :: Person
aProfessor =
  Person
    { personName = "John Atanasoff",
      personAge = Dead,
      personProfession = Professor (Physics Theoretical)
    }
```

Here are some examples of running `printPerson` with our values:

```haskell
> printPerson aPerson
"Victor Vega was a antagonist of Reservoir Dogs"
> printPerson aBackendProgrammer
"Kristina is a 24 years old backend programmer at Quanterall"
> printPerson aFrontendProgrammer
"Pesho is a 42 years old frontend programmer at Quanterall"
> printPerson aFullstackProgrammer
"Sasho is a 18 years old fullstack programmer at Quanterall"
> printPerson aProfessor
"John Atanasoff was a professor of theoretical physics"
```

## Generic datatypes

The most basic generic datatype is a type that can hold anything:

```haskell
data Holder a = Holding a
```

Note how we now have a type variable on the left side of `=` which means that when we refer to the
type it will take a type name. If we were holding a `Int`, for example, the type is `Holder Int`.
The corresponding constructor call (or pattern match) is `Holding value`.

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

Lists are useful, well, any time you need to have zero or more of something, obviously. It's
important to note that while Haskell lists are technically pointers to pointers to pointers, etc.
a lot of this inefficiency is mitigated by the fact that Haskell doesn't ask for the next value in a
list unless it needs it. This has to do with lazyness/non-strictness and is a language feature too
big to detail here.
