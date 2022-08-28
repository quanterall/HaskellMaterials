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
    - [`prop :: (HasCallStack, Testable prop) => String -> prop -> Spec`](#prop--hascallstack-testable-prop--string---prop---spec)
    - [`Testable prop`](#testable-prop)
    - [A more complete example](#a-more-complete-example)
    - [`Arbitrary`](#arbitrary)
    - [Exercises (Property testing)](#exercises-property-testing)

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
getAs :: (FromJSON a) => String -> IO (Either String a)
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
libraries in Haskell, one of which is called QuickCheck:

### `prop :: (HasCallStack, Testable prop) => String -> prop -> Spec`

`prop` is the property testing equivalent of `it`. We pass a description of what we are trying to
test as well as a `Testable` property that we want to test, and `Hspec` will run the required
`QuickCheck` invocations to fit it into our normal test suite.

### `Testable prop`

The `Testable` class is a type class that describes different expressions that qualify as being
property testable. What this means in practice is that we have several ways we can write our
property tests and that assertions as usual are usable in our property tests:

```haskell
describe "Lists" $ do
  describe "`reverse`" $ do
    prop "Reversing a list twice is `id`" $ \xs ->
      reverse @Int (reverse xs) `shouldBe` xs
```

In the example above you can see that we are passing a function to `prop`. `QuickCheck` will, in a
sense, look at the types of the arguments declared in this function and generate random values for
them. In this particular example we specify that the `a` we want in the type signature of `reverse`
is an `Int`, which means that `QuickCheck` will know to generate a random list of `Int`s.

### A more complete example

```haskell
import Qtility.Environment
import Qtility.Environment.Types
import Qtility.EnvironmentSpec.TestState
import RIO
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import System.Environment (setEnv)
-- Note how we import both `Hspec` and `QuickCheck` modules here.
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (PrintableString (..))
import Test.QuickCheck.PrintableNonEmptyString (PrintableNonEmptyString (..))

spec :: Spec
spec = do
  describe "`readEnvironmentVariable`" $ do
    prop "Can read any `Int` value from the environment" $ \x -> do
      let key = EnvironmentKey "ANY_INT"
      setEnv (_unEnvironmentKey key) (show x)
      readEnvironmentVariable @Int key `shouldReturn` x

    prop "Can read any `Double` value from the environment" $ \d -> do
      let key = EnvironmentKey "ANY_DOUBLE"
      setEnv (_unEnvironmentKey key) (show d)
      readEnvironmentVariable @Double key `shouldReturn` d

    prop "Can read any `Bool` value from the environment" $ \b -> do
      let key = EnvironmentKey "ANY_BOOL"
      setEnv (_unEnvironmentKey key) (show b)
      readEnvironmentVariable @Bool key `shouldReturn` b
```

If we try to generate a random text string to test with, we will run into a problem. We do not
allow empty values to be returned from the environment and our tests will randomly be generated
with empty strings. On top of that we are really only concerned with printable strings in our
functions, so we only want those to be generated for testing.

### `Arbitrary`

In order to generate values for our tests `QuickCheck` uses the `Arbitrary` type class:

```haskell
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
  shrink _ = []
```

The only thing we have to do when we want an `Arbitrary` instance is to figure out how to generate
our type via `Gen`. `QuickCheck` already has extensive understanding of many of the types that we
could use to build up our own types, so generally this becomes a matter of building up lists,
characters, etc. and then using them to construct our type.

Let's create a test for our `Text` and see what happens if we don't already have our custom type:

```haskell
prop "Can read any `Text` value that is not empty from the environment" $
  \s -> do
    let key = EnvironmentKey "ANY_TEXT"
    setEnv (_unEnvironmentKey key) s
    readEnvironmentVariable @Text key `shouldReturn` Text.pack s
```

The output:

```haskell
Failures:

  test/Qtility/EnvironmentSpec.hs:89:5:
  1) Qtility.Environment.`readEnvironmentVariable` Can read any `Text` value that is not empty from the environment
       uncaught exception: ReadEnvironmentVariableError
       ReadEnvironmentMissingValue (EnvironmentKey {_unEnvironmentKey = "ANY_TEXT"})
       (after 1 test)
         ""
```

We can guarantee that our string is not empty by generating a character plus a string, then
prepending the character onto the string:

```haskell
prop "Can read any `Text` value that is not empty from the environment" $
  \c s -> do
    let key = EnvironmentKey "ANY_TEXT"
        value = c : s
    setEnv (_unEnvironmentKey key) value
    readEnvironmentVariable @Text key `shouldReturn` Text.pack value
```

Now we run into another issue, however:

```haskell
Failures:

  test/Qtility/EnvironmentSpec.hs:94:9:
  1) Qtility.Environment.`readEnvironmentVariable` Can read any `Text` value that is not empty from the environment
       Falsifiable (after 8 tests and 4 shrinks):
         'a'
         "\NUL"
       expected: "a\NUL"
        but got: "a"
```

We can see that we are getting a `NUL` byte in our string, because that is indeed a valid character
in a string. Our tests are not concerned with these, however, so we would like a way to not
generate these. We can do this by defining a type to represent the kind of string we want as well
as an instance of `Arbitrary` for it:

```haskell
import Test.QuickCheck (Arbitrary (..), PrintableString (..), arbitraryPrintableChar)

newtype PrintableNonEmptyString =
  PrintableNonEmptyString {unPrintableNonEmptyString :: PrintableString}
  deriving (Eq, Show)

instance Arbitrary PrintableNonEmptyString where
  arbitrary = do
    c <- arbitraryPrintableChar
    -- `PrintableString` is an already existing type that will guide generation of
    -- only printable characters.
    (PrintableString cs) <- arbitrary
    c & (: cs) & PrintableString & PrintableNonEmptyString & pure
```

### Exercises (Property testing)

1. Write a function that uppercases the first character in a `Text` as well as property tests that
   verify that it does the correct thing.

2. Create types called `UpperCaseCharacter` and `LowerCaseCharacter` and instances of `Arbitrary`
   for them that will only generate characters of the correct casing.

3. Write a property test ensuring that `clamp` never returns values outside of its given range:

```haskell
clamp :: Ord a => a -> a -> a -> a
clamp lowerBound upperBound v = undefined
```

4. Create a type called `NonEmptyText` and an instance of `Arbitrary` for it.
