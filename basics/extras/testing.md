# Testing

- [Testing](#testing)
  - [`package.yaml`](#packageyaml)
  - [Unit tests](#unit-tests)
    - [`describe :: HasCallStack => String -> SpecWith a -> SpecWith a`](#describe--hascallstack--string---specwith-a---specwith-a)
    - [`it :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)`](#it--hascallstack-example-a--string---a---specwith-arg-a)
    - [`shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation`](#shouldbe--hascallstack-show-a-eq-a--a---a---expectation)
    - [`shouldReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation`](#shouldreturn--hascallstack-show-a-eq-a--io-a---a---expectation)
    - [`shouldThrow :: (HasCallStack, Exception e) => IO a -> Selector e -> Expectation`](#shouldthrow--hascallstack-exception-e--io-a---selector-e---expectation)
    - [`shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Expectation`](#shouldsatisfy--hascallstack-show-a--a---a---bool---expectation)
    - [Exercises (Unit tests)](#exercises-unit-tests)
  - [Testing effects](#testing-effects)
    - [Just test the effects](#just-test-the-effects)
    - [Mocking effects with type classes](#mocking-effects-with-type-classes)
      - [Exercises (Mocking effects with type classes)](#exercises-mocking-effects-with-type-classes)
  - [Property testing](#property-testing)
  - [Considerations for testing](#considerations-for-testing)
    - [Find a testable core](#find-a-testable-core)
    - [Encoding your effects as type classes](#encoding-your-effects-as-type-classes)

Testing is of course a pivotal part of development and just because we have access to a very
competent type system does not mean that we don't have to test our code.

## `package.yaml`

Before talking about setting up tests it can be useful to know how a Haskell project is usually set
up.

When we generate a Haskell project via `stack new my-project-name quanterall/basic` or any of the
other Quanterall templates, we get a package file that describes the different components of our
package.

Normally this includes:

- One or more executables, meaning the binaries we want to generate to execute our program
- A library, meaning the code where the functionality we could import into other packages goes, as
  well as the code that can be used in both your executables and your testing code
- A test suite, meaning the executable(s) that test our library code

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
`shouldX` functions.

`hspec` has a
[list of expectations](https://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/Test-Hspec-Expectations.html)
you can use to make assertions in your tests.

### `describe :: HasCallStack => String -> SpecWith a -> SpecWith a`

`describe` is used to create a section of our test suite. It takes a string describing the section
as well as an action to execute, which means we can pass a `do` block to it:

```haskell
describe "Environment handling" $ do
  describe "`readEnvironmentVariable`" $ do
    ...
```

### `it :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)`

`it` introduces a test case. It first takes a string describing what the test is supposed to be
testing and then an action to execute, that will contain assertions/`shouldX` functions:

```haskell
describe "Environment handling" $ do
  describe "`readEnvironmentVariable`" $ do
    it "returns the value of the environment variable" $ do
      ...
```

### `shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation`

`shouldBe` is used to compare two values for equality and show you the values if they don't match:

```haskell
describe "Environment handling" $ do
  describe "`readEnvironmentVariable`" $ do
    it "returns the value of the environment variable" $ do
      result <- readEnvironmentVariable "QUANTERALL_ENV"
      result `shouldBe` "development"
```

A version called `shouldNotBe` is also available:

```haskell
describe "Environment handling" $ do
  describe "`readEnvironmentVariable`" $ do
    it "returns the value of the environment variable" $ do
      result <- readEnvironmentVariable "QUANTERALL_ENV"
      result `shouldNotBe` "production"
```

### `shouldReturn :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation`

`shouldReturn` can be used when we want to compare the result in some `m` to a value, so that we
don't have to create an intermediate variable:

```haskell
describe "Environment handling" $ do
  describe "`readEnvironmentVariable`" $ do
    it "returns the value of the environment variable" $ do
      readEnvironmentVariable "QUANTERALL_ENV" `shouldReturn` "development"
```

A version called `shouldNotReturn` is also available:

```haskell
describe "Environment handling" $ do
  describe "`readEnvironmentVariable`" $ do
    it "returns the value of the environment variable" $ do
      readEnvironmentVariable "QUANTERALL_ENV" `shouldNotReturn` "production"
```

### `shouldThrow :: (HasCallStack, Exception e) => IO a -> Selector e -> Expectation`

`shouldThrow` can be used to check that an action will throw an exception matching a given
selector. The `Selector` type is really just a type alias for `e -> Bool`, which means we can pass
any function that will take our exception type and return `True`/`False`. Typical usage looks as
follows:

```haskell
describe "`readEnvironmentVariable`" $ do
  it "Fails with an error if the environment variable is not set" $ do
    readEnvironmentVariable @String (EnvironmentKey "NOT_SET")
      `shouldThrow` (== ReadEnvironmentMissingValue (EnvironmentKey "NOT_SET"))
```

Above we are of course taking advantage of `Eq` being defined for our exception so that we can just
compare the exception with `==`.

### `shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Expectation`

`shouldSatisfy` can be used to check that a value satisfies a given predicate:

```haskell
describe "Environment handling" $ do
  describe "`readEnvironmentVariable`" $ do
    it "returns the value of the environment variable" $ do
      result <- readEnvironmentVariable "QUANTERALL_ENV"
      result `shouldSatisfy` (`elem` ["development", "production"])
```

### Exercises (Unit tests)

1. Create a new project called `testing-sandbox` any way you want and make sure that it has a
   `testing-sandbox-test` component for running tests. Make sure that it has a `Spec.hs` file in
   it that will allow you to create files that end in `Spec.hs` and have them run automatically
   when you run `stack test --fast` in the project directory.

2. Create a failing test in a file called `test/LibrarySpec.hs` that will always fail. Run
   `stack test --fast` and see what happens.

3. Run `stack test --fast --file-watch` and modify your test to pass. What happens?

## Testing effects

When we want to test effectful code have a few possibilities.

### Just test the effects

First of all, we can set up our tests and test data in such a way where we can simply execute the
effectful code on real things an get real results. One example of this would be to have a testing
database that is used for tests, and resetting that database to a known state for each one. Another
would be to have test files in your project that are used for functions that need a filesystem.

While this is not always possible for all of our effects, it's something that needs to be considered
when we are testing effects. It can oftentimes be simpler to set up the environment around our
tests than it is to create all the necessary code infrastructure to mock that environment.

### Mocking effects with type classes

When we use [capability constraints](../11-capability-constraints.md) to be specific about our
effects, we can also take advantage of the fact that our effects now can have multiple
implementations for different contexts/monads.

Let's say that we want to mock our filesystem, for example, and in our case we really only want to
mock the `readFile` function. We can do this by first establishing a type class for file I/O and
then implementing that for both our application and testing contexts.

Let's say we started out with the following:

```haskell
-- | Loads a @.env@ file if it's available, changing the current environment. Throws
-- 'EnvironmentFileNotFound' if the environment file cannot be found.
loadDotEnvFile :: (MonadThrow m, MonadIO m) => EnvironmentFile -> m ()
loadDotEnvFile ef@(EnvironmentFile path) = do
  unlessM (Directory.doesFileExist path) $ throwM $ EnvironmentFileNotFound ef
  fileContents <- readFileUtf8 path
  let dotEnvValues = parseDotEnvFile fileContents
  liftIO $
    forM_ dotEnvValues $ \(key, value) -> do
      -- If there is an environment variable that has the wrong formatting, we'll get an
      -- @IOException@ here. We'll just ignore it and move on.
      setEnv (_unEnvironmentKey key) value `catchIO` const (pure ())
```

The above code checks for the existence of a file via `Directory.doesFileExist` and if it exists
we'll read the file, parse its contents and set the keys inside of the file in our shell
environment.

Let's write some basic tests for what the function needs to ensure in terms of the file system
access that it has:

```haskell
describe "Parsing .env files" $ do
  describe "`loadDotEnvFile`" $ do
    it "Should throw when we are trying to load a file that does not exist" $ do
      let testState = TestState {_testStateFiles = Map.fromList []}
      runRIO testState (loadDotEnvFile "doesNotExist.env")
        `shouldThrow` (== EnvironmentFileNotFound "doesNotExist.env")
```

So far, so good, but what happens when we add a test for a file that should exist?

```haskell
describe "Parsing .env files" $ do
  describe "`loadDotEnvFile`" $ do
    it "Should throw when we are trying to load a file that does not exist" $ do
      let testState = TestState {_testStateFiles = Map.fromList []}
      runRIO testState (loadDotEnvFile "doesNotExist.env")
        `shouldThrow` (== EnvironmentFileNotFound "doesNotExist.env")

    it "Should not throw when the file exists" $ do
      let testState = TestState {_testStateFiles = Map.fromList [("test.env", "")]}
      runRIO testState (loadDotEnvFile "test.env") `shouldReturn` ()
```

Our test fails:

```haskell
Failures:

  test/Qtility/EnvironmentSpec.hs:100:7:
  1) Qtility.Environment, Parsing .env files, `loadDotEnvFile`, Should not throw when the file exists
       uncaught exception: EnvironmentFileNotFound
       EnvironmentFileNotFound {_unEnvironmentFileNotFound = EnvironmentFile {_unEnvironmentFile = "test.env"}}

  To rerun use: --match "/Qtility.Environment/Parsing .env files/`loadDotEnvFile`/Should not throw when the file exists/"

Randomized with seed 750963619

Finished in 0.4742 seconds
29 examples, 1 failure

qtility> Test suite qtility-test failed
Completed 2 action(s).
Test suite failure for package qtility-1.3.0
    qtility-test:  exited with: ExitFailure 1
Logs printed to console
```

We now have a choice between adding the example file or mocking the file system. Let's introduce a
type class for talking about file read access:

```haskell
class (Monad m) => ReadFiles m where
  readFileM :: FilePath -> m Text
  doesFileExistM :: FilePath -> m Bool
  doesDirectoryExistM :: FilePath -> m Bool
  readFileBytesM :: FilePath -> m ByteString

instance ReadFiles IO where
  readFileM = readFileUtf8
  doesFileExistM = doesFileExist
  doesDirectoryExistM = doesDirectoryExist
  readFileBytesM = ByteString.readFile

-- type AppM = RIO App
instance ReadFiles AppM where
  -- `liftIO` is not necessary because `readFileUtf8` is defined for `MonadIO`
  readFileM = readFileUtf8
  doesFileExistM = doesFileExist
  doesDirectoryExistM = doesDirectoryExist
  readFileBytesM = ByteString.readFile

--
-- Here is our testing state and its associated monad, for which we implement `ReadFiles`
--
newtype TestState = TestState
  { _testStateFiles :: Map FilePath Text
  }
  deriving (Generic)

type TestM = RIO TestState

newtype TestState = TestState
  { _testStateFiles :: Map FilePath Text
  }
  deriving (Generic)

foldMapM makeLenses [''TestState]

instance ReadFiles TestM where
  readFileM path = do
    fileMap <- view testStateFiles
    fileMap
      & Map.lookup path
      & maybe
        (throwM $ mconcat ["No such file: ", path] & userError & errorTypeL .~ NoSuchThing)
        pure
  readFileBytesM path = do
    fileMap <- view testStateFiles
    fileMap
      & Map.lookup path
      & maybe
        (throwM $ mconcat ["No such file: ", path] & userError & errorTypeL .~ NoSuchThing)
        pure
      & fmap encodeUtf8
  doesFileExistM path = do
    fileMap <- view testStateFiles
    pure $ Map.member path fileMap
  doesDirectoryExistM path = do
    fileMap <- view testStateFiles
    pure $ Map.member path fileMap
```

In our functions we can now use `doesDirectoryExistM` and `readFileM` to read files and this will
transparently be mocked as using a map for our file system access in tests:

```haskell
-- | Loads a @.env@ file if it's available, changing the current environment. Throws
-- 'EnvironmentFileNotFound' if the environment file cannot be found.
loadDotEnvFile :: (MonadThrow m, MonadIO m, ReadFiles m) => EnvironmentFile -> m ()
loadDotEnvFile ef@(EnvironmentFile path) = do
  unlessM (doesFileExistM path) $ throwM $ EnvironmentFileNotFound ef
  fileContents <- readFileM path
  let dotEnvValues = parseDotEnvFile fileContents
  liftIO $
    forM_ dotEnvValues $ \(key, value) -> do
      -- If there is an environment variable that has the wrong formatting, we'll get an
      -- @IOException@ here. We'll just ignore it and move on.
      setEnv (_unEnvironmentKey key) value `catchIO` const (pure ())
```

Our test now succeeds:

```haskell
29 examples, 0 failures

qtility> Test suite qtility-test passed
Completed 2 action(s).
```

#### Exercises (Mocking effects with type classes)

1. Write a function that pulls down the contents of a web page and decodes them as a given
   structure based on the return value:

```haskell
getAs :: (FromJSON a) => String -> IO (Either String a)`
```

   Write a test for this function than ensures that we get a valid response for a web page that
   exists and when the data can be decoded as the following structure:

```haskell
data User = User
  { _userUsername :: Text,
    _userEmail :: Text
  }
  deriving (Eq, Show, Generic)
```

   Write a type class for making HTTP `GET` requests and getting a `ByteString` back. Modify your
   code to use this new type class and then make the needed test modifications to have your test
   behave as you need it for your tests to pass.

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
and split it out into a pure function. We know intuitively that pure functions, calculations from
one value another, are easily testable. Try to encode the piece you want to test as a matter of
input and output without involving any side effects.

### Encoding your effects as type classes

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
other function that accomplishes the same thing, and it specifies that the monad has support for
modifying mailing lists:

```haskell
handleSignup :: (MailingListModify m, MonadIO m) => User -> m ()
```

Our implementation for our normal application monad will look exactly the same as our previous one;
we call a function that very likely works in `IO`/`MonadIO` and that's it:

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
