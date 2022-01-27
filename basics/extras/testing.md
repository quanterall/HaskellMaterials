# Testing

- [Testing](#testing)
  - [`package.yaml`](#packageyaml)
  - [Unit tests](#unit-tests)
  - [Property testing](#property-testing)
  - [Considerations for testing](#considerations-for-testing)
    - [Find a testable core](#find-a-testable-core)
    - [Encode your effects as type classes](#encode-your-effects-as-type-classes)

Testing is of course a pivotal part of development and just because we have access to a very
competent type system does not mean that we don't have to test our code.

## `package.yaml`

When we generate a Haskell project via `stack new my-project-name quanterall/basic` or any of the
other Quanterall templates, we get a package file that describes the different components of our
package.

Normally this includes:

- One or more executables, meaning the binaries we want to generate to execute our program
- A library, meaning the code where the functionality we could import into other packages goes
- A test suite, meaning the executable(s) that tests our library code

```yaml
library:
  source-dirs: src

executables:
  mortred:
    main: Main.hs
    source-dirs: app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - mortred

tests:
  mortred-test:
    main: Spec.hs
    source-dirs: test
    ghc-options: -Wall
    dependencies:
    - mortred
    - hspec >=2.0.0
```

The package file above specifies that we will find our library code in `src/*`, that we have only
one executable, called `mortred` and that the `main` function for it is located in `app/Main.hs` and
that we have one test suite called `mortred-test` that has its entry point in `test/Spec.hs`.

The above pattern is what you would expect to see, more often than not. We can see this reflected in
the package file also for the [qtility](https://github.com/quanterall/qtility) library:

```yaml
tests:
  qtility-test:
    main: Spec.hs
    source-dirs: test
    ghc-options: -Wall
    dependencies:
    - qtility
    - hspec >=2.0.0
    - hedgehog
    - hspec-hedgehog
    - QuickCheck
```

If we now look at `Spec.hs` for this project we might be surprised by what we find:

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

This directive, in short, says to discover tests from the `test` directory or its sub-directories
and run them. Any file that ends in `Spec` will be run as a test suite and the function that will be
run from it has to be named `spec` and return something of type `Spec`:

```haskell
spec :: Spec
```

So let's look at some parts of the test suite of the `Qtility.Environment` module from `qtility`:

## Unit tests

When we want to create a simple unit test, all we need to do is use the `describe`, `it` and
`shouldEqual` functions:

```haskell
{-# LANGUAGE TypeApplications #-}

module Qtility.EnvironmentSpec where

import Qtility.Environment
import Qtility.Environment.Types
import RIO
import System.Environment (setEnv)
import Test.Hspec

spec :: Spec
spec = do
  -- This is a sub-categorization in your tests. You can create these `describe` sections in
  -- whatever ways you want. Here we've elected to just name the section/category after the function
  -- being tested.
  describe "`readEnvironmentVariable`" $ do
    it "Fails with an error if the environment variable is not set" $ do
      -- Note how we use the `shouldThrow` function in the infix position to get a more naturally
      -- flowing description of our expectation.
      readEnvironmentVariable @String (EnvironmentKey "NOT_SET")
        `shouldThrow` (== ReadEnvironmentMissingValue (EnvironmentKey "NOT_SET"))

    it "Succeeds if we set the variable first" $ do
      let key = EnvironmentKey "SET"
      setEnv (_unEnvironmentKey key) "VALUE"
      result <- readEnvironmentVariable @String key
      -- `shouldBe` checks for normal equality and is likely what you'll use in most cases.
      result `shouldBe` "VALUE"

    it "Can read `Text` values correctly" $ do
      let key = EnvironmentKey "TEXT"
      setEnv (_unEnvironmentKey key) "VALUE"
      -- Note how we can also use `shouldReturn` to say that a monadic actual should return a value
      -- matching the value on the right. This is the same thing as using `<-` and then checking the
      -- result with `shouldBe`.
      readEnvironmentVariable @Text key `shouldReturn` "VALUE"
```

In our terminal we will get the following output for these tests:

```spec
Qtility.Environment
  `readEnvironmentVariable`
    Fails with an error if the environment variable is not set
    Succeeds if we set the variable first
    Can read `Text` values correctly
```

The three test rows will be green if they pass. If we were to have a failure, it would look as
follows:

```spec
Qtility.Environment
  `readEnvironmentVariable`
    Fails with an error if the environment variable is not set
    Succeeds if we set the variable first FAILED [1]
    Can read `Text` values correctly
```

And below all the test results we can see the following:

```spec
Failures:

  test/Qtility/EnvironmentSpec.hs:29:7: 
  1) Qtility.Environment.`readEnvironmentVariable` Succeeds if we set the variable first
       expected: "VALUE"
        but got: "VALUUE"
```

## Property testing

Sometimes we want to prove an attribute or a property of our code, not just through readymade and
static examples, but by putting it through a series of randomized tests. We can do so via several
libraries in Haskell, one of which is called Hedgehog:

```haskell
{-# LANGUAGE TypeApplications #-}

module Qtility.EnvironmentSpec where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Qtility.Environment
import Qtility.Environment.Types
import RIO
import System.Environment (setEnv)
import Test.Hspec
import Test.Hspec.Hedgehog
```

```haskell
    it "Can read any `Int` value from the environment" $ do
      let key = EnvironmentKey "ANY_INT"
      -- We execute `hedgehog` here in order to work in this randomized input monad where we can use
      -- generators and create property tests.
      hedgehog $ do
        -- For all the values that an `Int` can be
        value <- forAll Gen.enumBounded
        -- Set our environment variable to the value we just generated. Note how the type
        -- application here of `Int` is what decides for `forAll Gen.enumBounded` that we want to
        -- generate an `Int` value.
        liftIO $ setEnv (_unEnvironmentKey key) (show @Int value)
        -- When we read the environment value back
        result <- liftIO $ readEnvironmentVariable key
        -- We should get the same value back
        result === value
```

When this test is run we will get the same kind of output as before if there are no errors. In the
case where we have an error, however, we'll see more interesting output:

```haskell
Failures:

  test/Qtility/EnvironmentSpec.hs:82:9: 
  1) Qtility.Environment.`readEnvironmentVariable` Can read any `Int` value from the environment
         ✗ <interactive> failed at test/Qtility/EnvironmentSpec.hs:82:9
           after 1 test and 1 shrink.
         
               ┏━━ test/Qtility/EnvironmentSpec.hs ━━━
            19 ┃ spec :: Spec
            20 ┃ spec = do
            21 ┃   describe "`readEnvironmentVariable`" $ do
            22 ┃     it "Fails with an error if the environment variable is not set" $ do
            23 ┃       readEnvironmentVariable @String (EnvironmentKey "NOT_SET")
            24 ┃         `shouldThrow` (== ReadEnvironmentMissingValue (EnvironmentKey "NOT_SET"))
            25 ┃ 
            26 ┃     it "Succeeds if we set the variable first" $ do
            27 ┃       let key = EnvironmentKey "SET"
            28 ┃       setEnv (_unEnvironmentKey key) "VALUE"
            29 ┃       readEnvironmentVariable @String key `shouldReturn` "VALUE"
            30 ┃ 
            31 ┃     it "Can read `Text` values correctly" $ do
            32 ┃       let key = EnvironmentKey "TEXT"
            33 ┃       setEnv (_unEnvironmentKey key) "VALUE"
            34 ┃       readEnvironmentVariable @Text key `shouldReturn` "VALUE"
            35 ┃ 
            76 ┃     it "Can read any `Int` value from the environment" $ do
            77 ┃       let key = EnvironmentKey "ANY_INT"
            78 ┃       hedgehog $ do
            79 ┃         value <- forAll Gen.enumBounded
               ┃         │ -9223372036854775808
            80 ┃         liftIO $ setEnv (_unEnvironmentKey key) (show @Int (value + 1))
            81 ┃         result <- liftIO $ readEnvironmentVariable key
            82 ┃         result === value
               ┃         ^^^^^^^^^^^^^^^^
               ┃         │ ━━━ Failed (- lhs) (+ rhs) ━━━
               ┃         │ - -9223372036854775807
               ┃         │ + -9223372036854775808
            83 ┃ 
         
           This failure can be reproduced by running:
           > recheck (Size 0) (Seed 11703610880303394415 16113510040565526611) <property>
         
Randomized with seed 2124290712
```

We can see from the output that Hedgehog has intelligently generated random values and that after it
has found an error, it also tries to find other values that cause the error. This is a very useful
attribute because it will allow you, very often, to find simpler inputs that display the issue you
are looking at. Despite the input to your functions being random, it will try to humanize the result
so that you can more easily work with them.

## Considerations for testing

While it's hard to recommend changing code in order to be able to test it, it can be very useful to
consider which code we can test more easily and to find a balance where we are able to work
intuitively with things and still be able to test them.

Many people have tried to work with what's colloquially called the
[*Command Pattern*](https://en.wikipedia.org/wiki/Command_pattern) and this can be hard to justify
if you're not already using it for other purposes. While it's true that this is a very easily tested
pattern, basing your entire design around it could be considered missing the point. It's worth
considering this type of pattern if you can see other upsides to it, however.

### Find a testable core

If you cannot readily test the entire functionality of something, find a more easily testable core
and split it out. We know intuitively that pure functions, calculations from one value another,
are easily testable. Try to encode the piece you want to test as a matter of input and output
without involving any side effects.

### Encode your effects as type classes

Let's say we have the following function:

```haskell
handleSignup :: (MonadIO m) => User -> m ()
```

The internal logic of this function is not super important, but let's say that we use MailChimp
behind the scenes to handle adding a user to a mailing list, among other things. If we wanted to
test this function, it could prove challenging. We could take the mailing list addition action
(presumably something like `addToMailingList :: Email -> m ()`) as a parameter and swap that action
out in tests. This can get very tedious if we have several effects and several places.

One model is instead to describe our mailing list effects as a type class:

```haskell
class MailingListModify m where
  addToMailingList :: Email -> m ()
```

The `handleSignup` function uses this `addToMailingList` function internally just as it would any
other function that accomplishes the same thing. Our implementation for our normal application monad
will look exactly the same as our previous one; we call a `MailChimp` function and that's it:

```haskell
instance MailingListModify AppMonad where
  addToMailingList = MailChimp.addToMailingList
```

For testing purposes we will implement a piece of state and a testing monad, that we will use to
implement a mocked version of the above:

```haskell
data TestState = TestState
  { mailingListRef :: IORef (Set Email)
  }

newtype TestMonad a = TestMonad {runTestMonad :: RIO TestState a}
  deriving (Functor, Applicative, Monad, MonadReader TestState, MonadIO)

instance MailingListModify TestMonad where
  addToMailingList email = do
    list <- asks mailingListRef
    liftIO $ modifyIORef' list (Set.insert email)
```

Our test monad implementation simply puts the requested e-mail address into a set that we reference
via a `IORef`. This allows us to later query the reference to see what is in it.

We want to test that `handleSignup` adds the user to the mailing list, so let's write a test for
that:

```haskell
module LibrarySpec where

import Library
import RIO
import qualified RIO.Set as Set
import Test.Hspec

spec :: Spec
spec = do
  describe "`handleSignup`" $ do
    it "should add a user to the mailing list" $ do
      mailingListRef <- newIORef Set.empty
      let testState = TestState {mailingListRef}
          user = User {_userName = "rickard", _userEmail = Email "rickard.andersson@quanterall.com"}
      runRIO testState $ runTestMonad $ handleSignup user
      readIORef mailingListRef `shouldReturn` Set.singleton (_userEmail user)
```

We now have a version of our function where we didn't need to change any of the runtime behavior of
it and fundamentally the interface has remained essentially the same; we've just added the fact that
we are modifying mailing lists as an explicit effect via type classes. This has then enabled us to
implement this functionality for a testing monad that we can use to inspect the results of our
effects.

It's important to keep in mind that when we test like this, we are not testing whether or not we can
add users to mailing lists, but rather that `handleSignup` adds users to mailing lists. We've
mocked the functionality of adding the user, so fundamentally that part is essentially useless to
test. We are testing the intent of `handleSignup` to add a user to the mailing list, not whether our
mailing list addition code works.
