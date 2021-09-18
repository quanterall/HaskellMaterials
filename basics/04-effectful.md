# "Effectful"

- ["Effectful"](#effectful)
  - [IO](#io)
  - [IO ()](#io-)
  - [IO a](#io-a)
    - [do-notation](#do-notation)
    - [Exercises (`IO a`)](#exercises-io-a)
      - [Exercise notes (`IO a`)](#exercise-notes-io-a)
  - [Basic error handling](#basic-error-handling)
  - [Making HTTP requests](#making-http-requests)
    - [Dependencies](#dependencies)
      - [Using `RIO`](#using-rio)
    - [Making a basic `GET` request](#making-a-basic-get-request)
    - [What if a HTTP request fails?](#what-if-a-http-request-fails)
    - [Dealing with JSON responses](#dealing-with-json-responses)
    - [Exercises (Making HTTP requests)](#exercises-making-http-requests)
  - [What makes `IO` special?](#what-makes-io-special)
  - [Should you avoid effectful things?](#should-you-avoid-effectful-things)

Lots of texts, these materials included, will talk about things being "effectful". So what does that
actually mean?

One way to view it is that an effectful thing can return different values at different times even
when given the same parameters, or cause something to happen externally, perhaps as the main purpose
of running the function.

## Note on chapter order

This chapter can essentially be worked through without reading [Type Classes](./03-type-classes.md),
but it is wise to read and understand the concepts of `Functor`, `Applicative` and `Monad` and how
they allow us to work generally with higher-kinded types. If one were to complete the exercises here
without doing that, it would be wise to return after these concepts are internalized to see how they
change the resulting code.

## IO

`IO` means that the function executing this has the capability to do effectful things. You can view
`IO` as a license to access basically all APIs. Executing in the "IO monad" can be viewed as having
full privileges to do whatever we want, be it reading/writing from/to a disk, talking to the
Internet or "launching the missiles", as people like to joke. For most programs, `IO` is where we
affect or gather data from reality and so it's where most interesting things happen.

## IO ()

`IO ()` is a fairly common type signature for effectful functions. What does it mean to have
something follow `IO` like this? It can be said that types can have arguments. In this particular
case we can say that `IO` has the "type" `IO :: Type -> Type`. When a type takes other types, the
type signature of it is actually called its "kind". If we execute the `:kind` command in our REPL
we can inspect what kind `IO` has:

```haskell
Q> :kind IO
IO :: * -> *
```

The asterisks here represent types: If we pass a type to `IO` we will get a concrete one back. This
means that `IO ()` is `IO` applied to `()` which produces the type `IO ()`.

Likewise we can also have `IO String` which is `IO` applied to `String`, which produces the type
`IO String`. `IO` itself can be seen as a type constructor in the type system, that requires a type
to be passed to it in order to construct a concrete one. When types take other types we call them
**"higher-kinded" types**. If this seems new, go back to the
[section on higher-kinded types](./03-type-classes.md#higher-kinded-types).

So what is it about `IO ()` that makes it so common in libraries and APIs? `()` is called "unit",
stemming from the fact that it is a type that has only one constructor: `()`. A value of type unit
can only be `()`:

```haskell
unit :: ()
unit = ()
```

`IO` means we have an essentially arbitrary action, and `()` is the type representing "No
interesting return value", which means that the closest analog we can find in other languages is
`void`. Functions that return `IO ()` are used because they cause something to happen and that's
their main purpose.

Examples:

```haskell
-- Output a string to the terminal, followed by a newline.
System.IO.putStrLn :: String -> IO ()

-- Create a directory in the file system
System.Directory.createDirectory :: FilePath -> IO ()

-- Remove a directory in the file system
System.Directory.removeDirectory :: FilePath -> IO ()

-- Flush the current GL context
Graphics.Rendering.OpenGL.GL.FlushFinish.flush :: IO ()

-- Set an environment variable to a certain value.
System.Environment.setEnv :: String -> String -> IO ()
```

As we can see, it's not uncommon for these functions to take parameters, but the main thing they
have in common is that they cause something to happen, which is likely the reason we're running them.
These can be the building blocks that our program uses behind the scenes in order to actually do
something in the end.

When a function returns `IO ()` we can put it on its own line without binding the result value and
Haskell, since it knows that `()` is deemed an unimportant return value, will not complain:

```haskell
import qualified System.Directory as Directory
import Prelude

main :: IO ()
main = do
  -- If this function returned `IO String`, we would get a type error saying that we were
  -- expecting `IO ()` but got `IO String`, since our return value in `main` is `IO ()`.
  Directory.createDirectory "new-directory"
```

## IO a

So what happens when we want to use functions that do effectful things but we also want to use
their return values? Their function signatures are going to have `IO a` at the end, where `a` stands
in for whatever thing the function returns. An example:

```haskell
-- Get the current value of an environment variable.
System.Environment.getEnv :: String -> IO String
```

We can see here that we are passing the function a `String` and getting an `IO String` back. We are
executing "in the IO monad", so this is something effectful that can do basically anything.

### do-notation

Haskell has special syntax/notation for using `IO` (and other monads). We get access to this
notation by starting an expression with the `do` keyword. We will go into the specifics of what `do`
here is actually doing in the next chapter, but suffice to say that you can consider this the way to
write code in the `IO` monad for now.

Technically speaking, when we have a **value** of type `IO a` we in actuality have an action that
when executed will produce a value of type `a`. When we use `<-` in our do-expressions we are
running that action and binding the **result**, the `a`, to the name on the left.

Let's look at an example with type signatures added just for extra clarity; these are not needed for
the code to work:

```haskell
import Prelude
import qualified System.Environment as Environment

main :: IO ()
main = do
  dockerFileName :: String <- Environment.getEnv "DOCKERFILE" :: IO String
  dockerFileContents :: String <- readFile dockerFileName :: IO String

  putStrLn dockerFileContents :: IO ()
```

Here is the same code without the type signatures:

```haskell
import Prelude
import qualified System.Environment as Environment

main :: IO ()
main = do
  dockerFileName <- Environment.getEnv "DOCKERFILE"
  dockerFileContents <- readFile dockerFileName

  putStrLn dockerFileContents
```

It's perhaps helpful to draw the analogy to `await` in JavaScript/TypeScript, where we sometimes
write our code "in the `Promise` monad" and so we can do asynchronous things. We unpack/evaluaten
these asynchronous values by using `await` (or `.then()` for people who aren't up-to-date) and when
we refer to them in code the asynchronous nature does not matter in terms of the values they
represent.

In reality `IO` and `Promise` are fundamentally the same in concept. What we actually have in our
program is a recipe or intent that, when evaluated or acted upon, will give us a value of some type.
We can write our code in such a way where we are saying we will evaluate that recipe and get the
value. In Haskell we use `<-` and in JavaScript/TypeScript we use `await`. The main difference here
is that `<-` is also useful for other monads.

For a concrete comparison of these two side by side, see [this file](../misc/typescript-await.md).

### Exercises (`IO a`)

1. Define a function that takes a file path and returns the lines[0] in the file (`[String]`). Use
   `readFile` for reading the file.

2. Define a function that takes a file path and returns the lines in the file that match a predicate
   that you pass to the function.

3. Define a function that takes a file path and returns the amount of characters, lines and words[1]
   in the file. Define a type to hold this information and return it from the function.

4. Define a function `filesInDirectory :: FilePath -> IO [FilePath]` that takes a path and returns
   all files in that path. You will need to determine what is a file and what isn't([2],[3],[4]).

5. Make the function that you created in exercise 3 work for an entire directory, returning a list
   of the type you defined. Use the function you defined in exercise 4 and see if you can find a
   function that allows you to run the function from exercise 3 on the list of files, returning a
   list of whatever type you return from that.

6. Define a data type `FileType` representing either a `Directory` or `File` and create a function
   that takes a path and returns all the paths inside of it as either `Directory`[5] or `File`,
   depending on their file type.

7. Make the function that you created in exercise 5 work for a given directory and all its
   sub-directories, recursively. Use the function from exercise 6 to figure out when you need to go
   deeper into a directory. Make sure that the path you are using in recursive calls is actually the
   complete path to a file. You can use `(</>)`[9] to combine paths together.

8. Define a function that gets the absolute path[10] for a given `path`, then returns a list of all
   preceding path segments([11],[12]) along with the full segment of the given path. For example:
   `pathSegments "./src"` in the development container should return
   `["/workspace/src" , "/workspace/" , "/"]`.

9. Define a function `findProjectRoot :: FilePath -> IO (Maybe FilePath)` that will search backwards
   starting from a given path, looking for a directory called `.git`. When it finds it, it returns
   the result. If it can't find a it, returns `Nothing`.

   Examples:

```haskell
Q> findProjectRoot "/bin/"
Nothing
Q> findProjectRoot ".stack-work/install/x86_64-linux/"
Just "/workspace/"
Q> findProjectRoot "src"
Just "/workspace/"
```

#### Exercise notes (`IO a`)

0. [`lines`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:lines)
1. [`words`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:words)
2. [`listDirectory`](https://www.stackage.org/haddock/lts-17.12/directory-1.3.6.0/System-Directory.html#v:listDirectory)
   lists all the files in the path given to it. `"."` can be used to refer to the current directory.
   Requires the package `directory`, add to `package.yaml` in the `dependencies` section.
3. [`doesFileExist`](https://www.stackage.org/haddock/lts-17.12/directory-1.3.6.0/System-Directory.html#v:doesFileExist)
   returns `True` for any path that is a file.
   Requires the package `directory`, add to `package.yaml` in the `dependencies` section.
4. [`filterM`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Control-Monad.html#v:filterM)
   allows one to filter a structure with a predicate that returns something monadic, like `IO Bool`.
5. [`doesDirectoryExist`](https://www.stackage.org/haddock/lts-17.12/directory-1.3.6.0/System-Directory.html#v:doesDirectoryExist).
6. [`traverse`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:traverse)
7. [`concat`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:concat)
8. [`foldMap`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Prelude.html#v:foldMap)
9. [`(</>)`](https://www.stackage.org/haddock/lts-17.12/filepath-1.4.2.1/System-FilePath-Posix.html#v:-60--47--62-).
   Concatenates two paths together, making sure there is a path separator between them.
   Requires the package `filepath`, add to `package.yaml` in the `dependencies` section.
10. [`makeAbsolute`](https://www.stackage.org/haddock/lts-17.12/directory-1.3.6.0/System-Directory.html#v:makeAbsolute).
11. [`inits`](https://www.stackage.org/haddock/lts-17.12/base-4.14.1.0/Data-List.html#v:inits)
    Returns a list of successively more segments of a given list. Example:

```haskell
Q> List.inits ["Research", "In", "Motion"]       
[[],["Research"],["Research","In"],["Research","In","Motion"]]
```

12. [`splitPath`](https://www.stackage.org/haddock/lts-17.12/filepath-1.4.2.1/System-FilePath-Posix.html#v:splitPath).
   Splits a given path on path separators, giving you the different components of the path.
   Requires the package `filepath`, add to `package.yaml` in the `dependencies` section.

## Basic error handling

If you've had the misfortune of running your exercise solutions on files that don't exist, or have
had other typical file system errors pop up, you'll have noted that Haskell has exceptions. Indeed
it's deducible from the types of many of the functions we've used that they will have to throw
exceptions on failure:

```haskell
-- This would have to be some kind of `Maybe String` or `Either errorType String`, we can't have a
-- `String` if it failed.
readFile :: FilePath -> IO String

-- Same issue; though we could use `[]` to signify error, but that's almost equally bad.
listDirectory :: FilePath -> IO [String]

-- Does this return a result if we don't have permissions to access the given path?
doesFileExist :: FilePath -> IO Bool
```

In order to take care of exceptions we can use `catch`:

```haskell
maybeReadAllLines :: FilePath -> IO (Maybe [String])
maybeReadAllLines filename = do
  -- Note here that we have to give a type signature to the `_e` parameter; Haskell uses this to
  -- determine what to catch.
  fileContent <- fmap Just (readFile filename) `catch` (\(_e :: IOException) -> pure Nothing)
  fileContent & fmap lines & pure
```

If we want to turn an `IOException` into an `Either` automatically, we can use
[`tryIO`](https://www.stackage.org/haddock/lts-18.10/unliftio-0.2.20/UnliftIO-Exception.html#v:tryIO)
from the [unliftIO package](https://www.stackage.org/lts-18.10/package/unliftio-0.2.20):

```haskell
maybeReadAllLines' :: FilePath -> IO (Either IOException [String])
maybeReadAllLines' filename = do
  tryIO $ lines <$> readFile filename
```

This can also be used in order to inspect the `IOException` after catching it, so we can possibly
return a much more descriptive and specific error:

```haskell
import GHC.IO.Exception (IOErrorType (..), IOException)
import Prelude
import System.IO.Error (ioeGetErrorType, isDoesNotExistError, isPermissionError)

data FileReadError
  = FileNotFound FilePath
  | PermissionsError FilePath
  | HardwareFaultError
  | UnknownFileReadError IOException
  deriving (Eq, Show)

maybeReadAllLines'' :: FilePath -> IO (Either FileReadError [String])
maybeReadAllLines'' filename = do
  result <- tryIO $ lines <$> readFile filename

  pure $ case result of
    Right lines' -> Right lines'
    Left e
      | isDoesNotExistError e ->
        Left $ FileNotFound filename
      | isPermissionError e ->
        Left $ PermissionsError filename
      | ioeGetErrorType e == HardwareFault ->
        Left HardwareFaultError
    -- The rest of the errors aren't covered; we likely want to handle "unknown"
    -- errors in a special way
    Left e ->
      Left $ UnknownFileReadError e
```

## Making HTTP requests

It's quite common to want to make HTTP requests in an application, so we'll take a look at a library
for doing just that. We'll be using the knowledge we've gotten so far and also see some new things.

### Dependencies

We will be using the following libraries:

- http-client
- http-client-tls
- http-types
- rio
- aeson

The first three are obviously related to making HTTP calls. We use this because it's a fairly basic
but competent interface to the concept of making HTTP calls.

`rio` is a standard library module that offers a lot of what we want when making an application out
of the box. It pulls in a lot of standard types and functions that we want access to.

`aeson` is a library for dealing with JSON data. It's by far the most common library. We will be
using it sparingly in these examples only to show that we can indeed couple our HTTP calls with
JSON decoding.

Add these dependencies to the `dependencies` section in your `package.yaml` file and run
`stack build`.

#### Using `RIO`

Replace `import Prelude` with `import RIO` and if you have a warning about `putStrLn` not being
imported, feel free to import it with `import System.IO (putStrLn)`.

### Making a basic `GET` request

The following code snippet shows how to make a basic request to a site. If you save this to your
editor and run it in the REPL (`stack repl`), you should see your IP printed to the REPL as a
string.

```haskell
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import RIO

-- We want our call to return a lazy bytestring. For now this is only for internal reasons as that's
-- the string type that we will have access to from the call.
getIpString :: IO LByteString
getIpString = do
  -- We are using the global TLS/HTTPS manager for these examples. In later examples we will be
  -- storing this in the application state instead.
  manager <- getGlobalManager :: IO Manager

  -- We parse a basic `Request` value from the base URL that we have.
  request <- parseRequest "https://ifconfig.co/" :: IO Request
  -- The site we are going to talk to returns info about the caller's IP address (and more)

  -- We modify the request by using record update syntax. The record we want to change a key in
  -- comes first, followed by curly braces and the field & value combination that we want. The
  -- resulting record, bound here to `requestWithHeaders` is a new record with that field changed.
  let requestWithHeaders = request {requestHeaders = [("User-Agent", "curl")]}
  -- In this particular example we are telling the site that we're actually the CLI tool/library
  -- `curl`, in order to get a nice plain, short response.

  -- We make the request and get an `IO Response` back. This response will be created with a
  -- "lazy bytestring" as the response body type.
  response <- httpLbs requestWithHeaders manager :: IO (Response LByteString)

  -- If we create a binding for `body` here it's easier to see what is going on. We get the body
  -- from the response with the `responseBody` function and get the lazy bytestring. This is just a
  -- value of type `LByteString` but we want to return `IO LByteString`. This means we need to put
  -- this value in the `IO` context somehow. For this purpose we use the `pure` function:
  -- `pure :: a -> IO a`
  let body :: LByteString
      body = responseBody response

  pure body
```

### What if a HTTP request fails?

What if we want to guard for failure somehow? HTTP can and will fail. Here is a version of
`getIpString` that allows for the possibility of failing at the response level. Note how we reflect
this possibility correctly in our return type:

```haskell
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import RIO

getIpStringMaybe :: IO (Maybe LByteString)
getIpStringMaybe = do
  manager <- getGlobalManager :: IO Manager
  request <- parseRequest "https://ifconfig.co/" :: IO Request

  let requestWithHeaders = request {requestHeaders = [("User-Agent", "curl")]}

  response <- httpLbs requestWithHeaders manager :: IO (Response LByteString)

  -- Our expected return type here is `IO (Maybe LByteString)` and again we find ourselves holding
  -- onto a value of type `LByteString` (`responseBody response`). We wrap our response body in a
  -- `Just` and wrap that in `IO` if we have a successful response, otherwise we just inject
  -- `Nothing` into `IO`.
  if statusIsSuccessful $ responseStatus response
    then pure $ Just (responseBody response)
    else pure Nothing
```

### Dealing with JSON responses

Quite often the data that we request comes back in the form of JSON responses. We'll try to provide
a basic example here with the expectation that we will delve into JSON a bit more deeply in later
chapters:

```haskell
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import RIO

-- This is not actually all of the JSON response from the endpoint, but rather just a few
-- interesting parts of it.
data IPGeoInfo = IPGeoInfo
  { ip :: String,
    country :: String,
    zip_code :: Maybe String,
    city :: Maybe String,
    latitude :: Float,
    longitude :: Float
  }
  -- Note that we are deriving `Generic` here. This is a way of telling the Haskell compiler to
  -- generate information about what the shape of a structure is so that it can be used to generate
  -- more code.
  deriving (Eq, Show, Generic)

-- This code snippet only works because we derived `Generic` for `IPGeoInfo`. We are implementing
-- the `FromJSON` type class with an instance that uses the type information that Haskell figured
-- out about our type. The JSON decoding will automatically be figured out based on that.
-- We can view this as `Generic` being the blueprint for constructing our type and this code saying
-- "Oh, I'll just use the blueprint that you've made sure exists".
instance FromJSON IPGeoInfo where
  parseJSON = genericParseJSON defaultOptions

getIpInfo :: IO (Either String IPGeoInfo)
getIpInfo = do
  manager <- getGlobalManager :: IO Manager
  -- We are now requesting the `/json` path from the server in order to get a JSON response
  request <- parseRequest "https://ifconfig.co/json" :: IO Request

  let requestWithHeaders = request {requestHeaders = [("User-Agent", "curl")]}

  response <- httpLbs requestWithHeaders manager :: IO (Response LByteString)
  if statusIsSuccessful $ responseStatus response
    then -- We are using `eitherDecode` here in case we are getting a successful response.
    -- `eitherDecode` takes a bytestring and attempts to decode it with a given `FromJSON`
    -- instance. Haskell knows that we are expecting a `IPGeoInfo` here, so it knows that it
    -- should use the `FromJSON` instance defined for it. It will attempt to decode each field as
    -- the correct type and if it succeeds, we will get a `Right` result.
      pure $ eitherDecode $ responseBody response
    else -- Since we are returning `Either String IPGeoInfo` we have to turn the error case here
    -- into a `Left`. The string chosen here is not particularly descriptive, but this example is
    -- really only for illustrative purposes.
      pure $ Left "Status code is not 200"
```

### Exercises (Making HTTP requests)

1. Define a function that fetches the `README.md` file of the `fpco/typed-process` repository on
   GitHub. Remember that raw file data can be fetched via the "Raw" links in GitHub.

2. Define a function that fetches **any** `README.md` file from a GitHub repository if it exists. If
   it doesn't, return `Nothing`.

3. Create a function `getAsCurl :: Url -> IO (Either Status LByteString)` that automatically adds
   the User-Agent "curl" to a request. Make `Url` a newtype.

4. Define a function that uses the "repositories" API of GitHub to get all the contributors to a
   repository. Have the function return a list of contributors with their login and number of
   contributions. **Note**: You have to have a "User-Agent" header in calls to the GitHub API. You
   can use your `getAsCurl` function to guarantee that you are sending the "curl" user agent.

   You can see the documentation for the GitHub API
   [here](https://docs.github.com/en/rest/reference/repos#list-repository-contributors).

5. Define a function that gets all the public repositories of a user. You will need to create a type
   that represents what a repository is. Remember that you only need to define the fields you care
   about; you can leave all the others out.

   See [this link](https://docs.github.com/en/rest/reference/repos#list-repositories-for-a-user) for
   documentation on how to get the repositories of a user.

   **Note**: Don't worry about pagination; if a user has too many repositories for them all to be
   returned in one call, just consider this outside of the scope of this exercise.

## What makes `IO` special?

In reality, nothing. `IO` isn't really the bit that's special. Every program you've ever written in
a language that didn't have this concept was always basically running in the `IO` monad, except we
usually do not have access to the actions we execute **as values** and we generally don't talk
about these actions in the type system of whatever language we're using. When you write
`putStrLn "hello"` in Haskell, you are in fact creating a value. Passing that value around is
trivial, so it can be used in other functions. Fundamentally speaking, however, `IO` as "a context
in which we can do whatever we want" is not the part that should jump out at you as new territory.

It's perhaps more interesting that in Haskell we are able to say that certain functions **can't** do
these interesting things; they're only for computing values. This means that we can now definitively,
in our APIs, say that a callback is not able to talk to the network, for example, or do its own
logging.

The following is an illustrative example:

```haskell
maybeReadEvent :: (ByteString -> Maybe Message) -> Socket -> IO (Maybe Message)
maybeReadEvent messageDecoder socket = do
  ...
```

Since our first argument doesn't have the return type `IO (Maybe Message)` the only thing it can do
is either produce nothing from a given byte string, or produce a value of type `Message`. This is a
sensible design choice for a decoding function, and one we can make explicit in our API. Attempting
to do effectful things in this function will lead to using functions like `unsafePerformIO` and
friends, making it clear that one is outside of the realm of reasonable usage.

## Should you avoid effectful things?

It's a bit of a meme that Haskell programmers avoid or dislike effectful things. This is overblown
and in reality nothing useful ever gets done without at some point executing in `IO` or some context
that wraps it. With that in mind, it's still the case that pure functions can be used everywhere,
whereas the possible usage of impure functions will always depend on the context we're in.

Should a function meant to validate a data type execute in `IO`? Probably not. Common sense prevails
here and software is iterative; you will be able to see what can be made pure and thus less
mysterious in time. Making functions pure is not a chore to be done to appease the Haskell gods, but
is mostly a question of removing future questions in future debugging sessions.
