# Modules

This file will explain things that you'll likely be faced with when opening a module; the structure,
restrictions/guidelines on naming, language extensions, imports and so on.

## Language extensions

Usually what you will see when opening many modules are language extensions. These are things that
aren't defined in the "base" language but have been added over time. Haskell has many of these and
lots of them are used fairly often.

The Haskell2010 declaration you can find in project files is a short-hand for saying that you want
the language extensions they put as defaults in the 2010 edition of Haskell turned on.

`OverloadedStrings` is probably the most common language extension in Haskell, it allows strings
to be used as constant values for anything that can be turned into a string. This helps make
constant values easier to deal with for the 3 different string types that Haskell has, among other
things. 

The nature of language extension is that they generally either allow new syntax to be used for
making things more convenient or they remove a restriction from the type system that has been
determined to be warranted when asked for. They vary in power and usefulness and it's important to
consider that adding language extensions isn't always a non-issue.

A team should likely have a set of acceptable language extensions to use and have them activated by
default in their project files, as well as have conversations about ones that could potentially be
added without increasing the cognitive load of working with the code base.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
```

Other common language extensions used in many places are, among others:

- `DeriveGeneric`
- `GeneralizedNewtypeDeriving`
- `InstanceSigs`
- `FlexibleContexts`
- `FlexibleInstances`
- `MultiParamTypeClasses`
- `ScopedTypeVariables`
- `TypeApplications`

## The `module` keyword

What follows after language extensions is a module declaration:

```haskell
module Project.ExampleModule
  -- These are exports
  ( publicFunction,
    publicFunction2,
    value,
    value2,
    -- We export all constructors for this type
    OurCustomType (..),
    -- We export no constructors for this type
    OurOtherCustomType,
    addIntegers,
    addFloats,
    addDoubles,
    -- This exports a module that was imported in the file
    module Project.OtherModule
  )
```

The module name here is related to the place where the file is located; the `Project` bit is a
directory in our top-level `src` directory. If our source root for a library is set to `src` this
module would be located in `src/Project/ExampleModule.hs`.

### Module naming conventions in Haskell

If one were to develop a library for something related to networking, it's very common in Haskell to
namespace the parts that have to do with this in the `Networking` namespace; thus we would define
modules with names like `Network.OurLibrary.*`. As an example, the "Web Application Interface",
`wai` is namespaced in `Network.Wai.*`, the database persistence library `persistent` is namespaced
in `Database.Persist.*`.

| Module name                      | Path*                                   |
| -------------------------------- | --------------------------------------- |
| `Network.Wai.Internal`           | `src/Network/Wai/Internal.hs`           |
| `Database.Persist.Sql.Migration` | `src/Database/Persist/Sql/Migration.hs` |
| `Network.Wai.Internal`           | `src/Network/Wai/Internal.hs`           |

(*) Note here that `src` is whichever directory you've set as being the root source directory for a
given component in your project file.

It's important to note that an application is unlikely to follow this convention. When something is
a usable library and meant for general consumption it's likely a good idea to follow this convention
but there is no point in namespacing your networked application under `Network`, for example.

## Imports

After our module name comes our imports:

```haskell
-- This imports the `Maybe` type with all its constructors.
import Data.Maybe (Maybe(..))
-- This imports the `Either` type without its constructors; if we only want to use the type name.
import Data.Either (Either)
-- Imports can be qualified, which means we have to specify the name after "as" in order to refer to
-- their contents. This makes for very clear origins to functions in our code and should be used
-- when it doesn't make your code awkward. Operators are usually not super nice with qualifiers, and
-- it's very common to use types unqualified.
import qualified RIO.Directory as Directory
-- We can also import *both* a qualified and unqualified version of a module
import RIO.Map (Map)
import qualified RIO.Map as Map
-- `Text` here is referring to a type
import RIO.Text (Text)
-- This imports everything in `Prelude`
import Prelude
```

Generally speaking it's a good idea to qualify your imports and if that becomes awkward, use
unqualified imports with specific import lists. It's very common to import operators unqualified
because they **do** become awkward very fast when you have to type too much.

A solution to possible import duplication is to have a core module that re-exports common modules in
your project, which means you can then import that module to get all of those things. It should be
noted that it's easy to create for confusing dependency structures when being too creative with
imports and too undisciplined with imports in general.
