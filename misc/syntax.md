# Haskell syntax

```haskell
-- What follows is a language extension. These are things that aren't defined in the "base" language
-- but have been added over time. Haskell has many of these and lots of them are used fairly often.
-- The Haskell2010 declaration you can find in project files is a short-hand for saying that you
-- want the language extensions they put as defaults in the 2010 edition of Haskell turned on.
--
-- OverloadedStrings is probably the most common "language extension" in Haskell, it allows strings
-- to be used as constant values for anything that can be turned into a string. This helps make
-- constant values easier to deal with for the 3 different string types that Haskell has, among
-- other things.
{-# LANGUAGE OverloadedStrings #-}

-- Other common language extensions used in many places are, among others:
--
-- DeriveGeneric
-- GeneralizedNewtypeDeriving
-- InstanceSigs
-- FlexibleContexts
-- FlexibleInstances
-- MultiParamTypeClasses
-- ScopedTypeVariables
-- TypeApplications
--
-- These all enable the source code to be interpreted slightly differently or remove some limitation
-- that the original Haskell language specification declared. Most of these are very common and safe
-- to use, but one should be aware that language extensions are not always a good idea to turn on
-- and it's good to keep a standard set of allowed extensions vs. disallowed ones.

module ModuleName
  ( publicFunction,
    publicFunction2,
    value,
    value2,
    OurCustomType (..),
    addIntegers,
    addFloats,
    addDoubles,
  )
where

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

-- This is a comment that exists in source only, it is not for documentation.

-- | This is a documentation comment that documents the thing that follows.
-- It can have several rows.
-- We can link to other pieces of documentation: "Data.String". This will link to the "Data.String"
-- module that we have in scope, which means we get up-to-date documentation based on our actual
-- dependencies.
--
-- We can also put examples in our documentation:
--
-- >>> value
-- "String value"
value :: String
value = "String value"

value2 :: Int
value2 = 42
-- ^ This is a documentation comment that documents the thing that comes before.

-- Functions are (usually) written `functionName :: type` and followed by their definition on the
-- next line. `::` means the declaration of a type in other contexts as well.
--

-- | `Int` here is a 32bit integer number. For so called 'big-integers' one can use `Integer`.
--
-- >>> addIntegers 42 1337
-- 1379
addIntegers :: Int -> Int -> Int
-- The last thing in the function's type is the return value. What comes before are arguments, in
-- order.
addIntegers a b = a + b

-- | `Float` here is (normally) a 32bit floating point number.
--
-- >>> addFloats 42 1337
-- 1379.0
addFloats :: Float -> Float -> Float
addFloats a b = a + b

-- | `Double` here is (normally) a 64bit floating point number.
--
-- >>> addDoubles 42 1337
-- 1379.0
addDoubles :: Double -> Double -> Double
addDoubles a b = a + b

-- Lists of things is written as square brackets surrounding the type, which is a special case
-- for containers in terms of types.
addOneToList :: [Int] -> [Int]
-- Lambdas/anonymous functions are written with an initial backslash, followed by the parameters
-- of the function, an arrow and the body expression of the function.
-- If we had several parameters it would be `\x1 x2 x3 -> ...`
addOneToList ints = map (\x -> x + 1) ints

data OurCustomType
  = CaseOne -- Holds no values, the tag/constructor itself is the only thing we have.
  | CaseTwo Int -- Holds an int value, we can use this when we have a `CaseTwo`.
  | CaseThree (Maybe String) -- Holds either `Nothing` or `Just string`, need to inspect to see what.
  | CaseFour
  | CaseFive (Map Text [Int])
  -- We can "derive" functionality for our types; this means we are automatically generating
  -- functions for the type, in this case comparison functions and the capability to turn the type
  -- into a string, the `show` function: `show :: OurCustomType -> String`.
  deriving (Ord, Show)

-- The `IO` type means that something is effectful. This has nuances and can be expanded on to be
-- more general than just `IO` but one can think of it as meaning:
-- "May return different results each time it's called.".
-- The `()` type is called "unit" and the only value that belongs to this type is `()`. It's very
-- commonly used to reflect that a function doesn't return anything interesting at all, here used
-- because this is something effectful that doesn't return anything.
publicFunction :: OurCustomType -> IO ()
-- We can pattern match in the "top-level", i.e. in the actual arguments declaration of functions.
publicFunction CaseOne = pure ()
-- We can use "do-notation", this is a way of writing multiple monadic expressions. We can also have
-- them return values and automatically bind these to values that we use later in the flow.
publicFunction (CaseTwo intValue) = do
  -- This returns a `FilePath`, a string representing a path.
  currentDirectory <- Directory.getCurrentDirectory
  -- When a monadic expression returns `SomeType ()` we don't need to bind the value to a variable.
  print intValue
  -- `$` is the application of the function on the left to the entire right side, which means the
  -- following is: `print ("We are currently in: " <> currentDirectory)`.
  -- `<>` is the concatenation of two values, as long as the type supports it.
  print $ "We are currently in: " <> currentDirectory
-- This matches on the empty string specifically, inside of a `Just`.
publicFunction (CaseThree (Just "")) = pure ()
-- This matches on any other string value.
publicFunction (CaseThree (Just anyOtherStringValue)) =
  putStrLn anyOtherStringValue
-- This matches on when we have no value embedded, i.e. `Nothing`.
publicFunction (CaseThree Nothing) = pure ()
publicFunction CaseFour = pure ()
publicFunction (CaseFive textMap) = print $ Map.size textMap

-- We can also match via the `case` keyword, which is a lot like a `switch` that can also
-- destructure data types.
publicFunction2 :: OurCustomType -> Bool
publicFunction2 valueForTheParameter = case valueForTheParameter of
  CaseOne -> True
  CaseFour -> True
  CaseTwo _ -> False
  CaseThree (Just _) -> True
  CaseThree _ -> False
  CaseFive textIntMap ->
    -- A function can be put in an infix position even though it's not an operator by surrounding it
    -- in double quotes. Here we are applying the `member` function to the map, asking if "someKey"
    -- is a key of the map (`Map Text Int`). This is a very common use case for this language
    -- feature. Note here that we are just using a constant string, but it's automatically converted
    -- into a value of type `Text`; this is because we are using `OverloadedStrings`.
    "someKey" `Map.member` textIntMap

-- Instance definitions are definitions meant to encode a certain behavior, a "type class", for a
-- given type. Here we are defining what it means for `OurCustomType` to have an equality operator,
-- which means we can use `==` for the type.
instance Eq OurCustomType where
  CaseOne == CaseOne = True
  CaseOne == _ = False
  -- Operators can be defined in a regular function position by surrounding them with parentheses.
  -- This also works when applying them, which means you can write `1 + 1` as `(+) 1 1`.
  (==) (CaseTwo intA) (CaseTwo intB) =
    intA == intB
  (==) (CaseTwo _) _ =
    False
  (CaseThree maybeStringA) == (CaseThree maybeStringB) =
    maybeStringA == maybeStringB
  (CaseThree _) == _ = False
  CaseFour == CaseFour = True
  CaseFour == _ = False
  (CaseFive textIntMapA) == (CaseFive textIntMapB) =
    textIntMapA == textIntMapB
  (CaseFive _) == _ =
    False
```