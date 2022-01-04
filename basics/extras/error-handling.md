# Error handling

- [Error handling](#error-handling)
  - [Types of error signalling](#types-of-error-signalling)
    - [Errors as "just values"](#errors-as-just-values)
    - [Type signatures that don't tell us the whole truth](#type-signatures-that-dont-tell-us-the-whole-truth)
    - [`(MonadThrow m) => m a`](#monadthrow-m--m-a)
      - [Downsides](#downsides)
    - [When should we do what?](#when-should-we-do-what)
    - [Tools for working with exceptions and values](#tools-for-working-with-exceptions-and-values)
      - [`try`](#try)
        - [`try :: (MonadUnliftIO m, Exception e) => m a -> m (Either e a)`](#try--monadunliftio-m-exception-e--m-a---m-either-e-a)
        - [`tryAny :: MonadUnliftIO m => m a -> m (Either SomeException a)`](#tryany--monadunliftio-m--m-a---m-either-someexception-a)
        - [`tryIO :: MonadUnliftIO m => m a -> m (Either IOException a)`](#tryio--monadunliftio-m--m-a---m-either-ioexception-a)
      - [`mapException`](#mapexception)
        - [`mapException :: (Exception e1, Exception e2) => (e1 -> e2) -> a -> a`](#mapexception--exception-e1-exception-e2--e1---e2---a---a)
        - [`mapExceptionM :: (Exception e1, Exception e2, MonadUnliftIO m) => (e1 -> e2) -> m a -> m a`](#mapexceptionm--exception-e1-exception-e2-monadunliftio-m--e1---e2---m-a---m-a)
      - [`fromX`](#fromx)
        - [`fromEither :: (Exception e, MonadThrow m) => Either e a -> m a`](#fromeither--exception-e-monadthrow-m--either-e-a---m-a)
        - [`fromEitherM :: (Exception e, MonadThrow m) => m (Either e a) -> m a`](#fromeitherm--exception-e-monadthrow-m--m-either-e-a---m-a)
        - [`fromMaybeM :: (Exception e, MonadThrow m) => e -> m (Maybe a) -> m a`](#frommaybem--exception-e-monadthrow-m--e---m-maybe-a---m-a)
      - [`catch`](#catch)
        - [`catch :: (MonadUnliftIO m, Exception e) => m a -> (e -> m a) -> m a`](#catch--monadunliftio-m-exception-e--m-a---e---m-a---m-a)
        - [`catchIO` & `catchAny`](#catchio--catchany)
        - [`handle`, `handleIO` and `handleAny`](#handle-handleio-and-handleany)
        - [`catches`](#catches)
      - [`bracket`](#bracket)
    - [Reading more](#reading-more)
    - [Additions to be made in the future](#additions-to-be-made-in-the-future)

Error handling is of course a central part of programming. Determining which errors are recoverable,
unrecoverable, common, rare or have other important attributes is something that we constantly come
into contact with, even if we might not be aware of it.

On top of this Haskell has a more interesting relationship to error handling than many languages,
which can be quite confusing. What I mean by this is that while Haskell users quite often prefer
dealing with errors as values, it's also an unavoidable certainty if one does interesting things
with Haskell that one has to deal with **exceptions**.

## Types of error signalling

### Errors as "just values"

When we talk about errors as values, what we actually mean is that we have translated some kind of
error condition into a value that has to be handled at the call site by the user of a function. As
an example of this, consider the following function:

```haskell
divideInt :: Int -> Int -> Maybe Int
divideInt x y = if y == 0 then Nothing else Just (x `div` y)
```

Note here that we're pre-checking the condition we're interested in guarding against. We're then
turning this condition into a specific return value, and the caller will have to inspect the return
value to see if it's a valid result or not. The same applies to functions returning a value of type
`Either errorType a`. In this case, the caller will have to inspect the `Either` value to see if
there is a `Left` with some attached error information or not. Otherwise they know that the call
succeeded and can use the success value.

For pure code, this is a fairly solid approach. Impure exceptions **are** rare and we shouldn't
necessarily be too cautious and expect to catch too many exceptions in pure code (and we should
never expect to throw exceptions in pure code).

There are scenarios where this model starts falling apart a bit, however.

### Type signatures that don't tell us the whole truth

Consider the following function:

```haskell
tryStartSession ::
  (MonadUnliftIO m) =>
  SessionMode ->
  SeleniumPath ->
  m (Either SessionStartError SessionStartResult)
tryStartSession sessionMode =
  startSession sessionMode >>> try
```

We can see here that our function should return two primary things:

- A `Right` value, which is a `SessionStartResult`; this is the success case of this function
- A `Left` value, which is a `SessionStartError`; this is the failure case of this function

When we use this function we have to do the following if we want to move forward with using a
potential success value:

```haskell
-- `maybeSeleniumProcess` here represents an uncertainty about the result of our call
maybeSeleniumProcess <-
  tryStartSession SessionOnDemand $ SeleniumPath "./selenium-server-standalone-2.53.1.jar"
case maybeSeleniumProcess of
  -- At this point we've done a case analysis of the value and determined that we succeeded in
  -- starting the session. We are pattern matching here on a specific type of success value.
  Right (StartedOnDemand seleniumProcess) -> do
    waitRunSession (Milliseconds 10000) (seleniumProcess ^. spPort & webdriverConfig) doScrape
      `finally` stopSession seleniumProcess
  -- Likewise we are also certain that we succeeded in this case, because we have a `Right`.
  Right (PremadeSession port) -> do
    waitRunSession (Milliseconds 10000) (webdriverConfig port) doScrape
  -- In this case we know that the call failed, because it returned `Left`. The error value here
  -- is for us to decode what to do with. In this particular case we actually just `throwM` the
  -- value, but it's conceivable that if this was a recoverable error we'd just retry the action
  -- or employ some kind of backup plan.
  Left err -> do
    throwM err
```

The above might seem clean, but there is an omission of the whole truth hidden in the type signature
of `tryStartSession`. While we can definitively say that once we're in the `Right` case we've
succeeded in creating a session and we can definitely say that if we have a `Left`, we've caught an
exception, it's not true that we will always have an error value.

The `tryStartSession` function **can throw**. The thing that we are actually saying when we're using
`try` with an `Either errorType a` is that if an exception of type `errorType` is thrown, we will
catch it and turn it into a value. If any other exception happens, it won't be caught and it'll
bubble up to the caller.

This happens because exceptions are inherently tied to types in Haskell. On top of that, exceptions
use a sub-typing system that otherwise is not present in Haskell. All exceptions are sub-types of a
type called `SomeException`. If we were to use `tryAny`, we would catch all exceptions, but we would
have an exception of precisely this type `SomeException`.

So when we're using `try` to turn this exception into an
`Either SessionStartError SessionStartResult`, we're very specifically saying that we'll catch only
this type of exception and turn it into a `Left` value.

We could also use `tryIO` to catch any `IOException` and turn it into a `Left` value. Much like our
very specific user-defined exception type, this would not catch just any exception, but rather just
`IOException`.

With the above in mind, the criticism that error handling with exceptions "lies" about its return
values becomes hard to reconcile. If you think back to other languages, even the ones using
primarily error values, it's usually a safe bet that you'll find that they, too, have some manner of
exceptions or exception-like language feature that likewise causes this type of "lie".

So what is the most "honest" type signature in the end?

### `(MonadThrow m) => m a`

In many cases code can actually be improved by using `m a` instead of `m (Either errorType a)`. This
often results in more easily composed code and it's trivial for a caller to turn `m a` into
`Either errorType a` themselves by using `try`.

If we employ this approach instead, the function we call looks as follows:

```haskell
startSession ::
  (MonadThrow m, MonadUnliftIO m) =>
  SessionMode ->
  SeleniumPath ->
  m SessionStartResult
startSession SessionOnDemand seleniumPath = do
  xvfbProcess <- mapExceptionM XvfbSessionError startXvfb
  (StartedOnDemand <$> startSelenium xvfbProcess seleniumPath)
    `catch` ( \(e :: SeleniumStartError) -> do
                xvfbProcess ^. xpProcess & stopProcess
                throwM $ SeleniumSessionError e
            )
startSession (SessionAlreadyStarted seleniumPort) _seleniumPath = do
  pure $ PremadeSession seleniumPort
```

The calling code now becomes:

```haskell
-- `startResult` will just be a `SessionStartResult` here
startResult <- startSession SessionOnDemand $ SeleniumPath "./selenium-server-standalone-2.53.1.jar"
-- We're really only concerned with the different types of `SessionStartResult` we can have here
case startResult of
  (StartedOnDemand seleniumProcess) -> do
    waitRunSession (Milliseconds 10000) (seleniumProcess ^. spPort & webdriverConfig) doScrape
      `finally` stopSession seleniumProcess
  (PremadeSession port) -> do
    waitRunSession (Milliseconds 10000) (webdriverConfig port) doScrape
```

In this case we've obviously not lost any reliability, as we were throwing the error anyway. We've
decluttered the calling code somewhat.

#### Downsides

One of the most obvious downsides of this approach is that we're not signalling at all what the
expected error type is. Though, it's of course always the case, as illustrated in the previous
section, that we can **say** that we expect some type of error, but still throw a completely
different one that we're not handling. Any promise that we're throwing or returning a specific one,
then, is more of a guarantee that we can throw/return that one (**among others**).

### When should we do what?

It's a reasonable assumption that not all errors are exceptional. When an error can be expected to
happen fairly often and is not part of some edge case, we should likely encode that scenario in an
error value. This suggests to the caller that they need to examine the value they get back to check
for this (fairly) common error scenario as part of normal usage. It's of course fine that
exceptional/rare errors still can be thrown and thus handled via the exception machinery, but we've
at least stated up front that we have some particular type of errors that we wouldn't be surprised
to see.

If code is part of initialization, it's not unreasonable to assume that it can use exceptions to
signal failure. If most applications are going to use library functionality specifically during
setup, we should feel free to make it use exceptions. If a user needs error values from them for
a specific set of errors, they should feel free to use `try`/`catch` to capture these scenarios into
types/handlers.

### Tools for working with exceptions and values

We saw earlier in this document that we can take code throwing exceptions and capture exceptions
into `Either errorType a` values. These functions come in three primary flavors:

#### `try`

`try` and its variants allow you to take functions throwing exceptions and turn errors into values,
as long as you have some idea of what you want to catch.

##### `try :: (MonadUnliftIO m, Exception e) => m a -> m (Either e a)`

Runs an action and if an exception of type `e` is thrown, this returns a `Left e` value. The way you
use `e` is what will determine what type of exceptions are going to be caught.

##### `tryAny :: MonadUnliftIO m => m a -> m (Either SomeException a)`

Returns an `Either SomeException a` value. Our usage of the `Left` value is always going to be the
`SomeException` parent type of all exceptions, so there is no type-directed catching; it just
catches all exceptions.

##### `tryIO :: MonadUnliftIO m => m a -> m (Either IOException a)`

Returns an `Either IOException a` value. This is the same as `tryAny` but for `IOException`.
`IOException` is a narrower type than `SomeException` and thus can be used to catch more specific
exceptions.

#### `mapException`

`mapException` and `mapExceptionM` allow us to just transform caught exceptions to other types,
usually into a bigger union type or the like.

##### `mapException :: (Exception e1, Exception e2) => (e1 -> e2) -> a -> a`

Takes a function that converts from one exception to another as well as the code to run. If an
exception is thrown and the type of that exception matches `e1`, the provided transformation
function will be applied to the exception and the result will be thrown. If the thrown exception
does not match, it will just be thrown.

##### `mapExceptionM :: (Exception e1, Exception e2, MonadUnliftIO m) => (e1 -> e2) -> m a -> m a`

This works like `mapException`, except we catch exceptions in a monadic context, as long as it has
an instance of `MonadUnliftIO`.

Let's look at a snippet of part of this in action:

```haskell
-- | Error representing a failure to set up @chromedriver@.
data ChromeDriverSetupError
  = -- | Unable to find @google-chrome@, which means we don't know which @chromedriver@ version to
    -- download.
    ChromeBinaryNotFound
  | -- | The chrome version we have has no valid @chromedriver@ version to download.
    NoValidChromeDriverUrl ChromeVersion
  | -- | The extracted major version of @google-chrome@ or @chromedriver@ is not supported.
    UnsupportedMajorVersion Text
  | -- | Unable to read the output of @google-chrome --version@.
    BadChromeVersionOutput LByteString
  | -- | Something went wrong when getting the output of @google-chrome --version@.
    UnableToReadChromeProcess FilePath IOException
  | -- | Something went wrong when downloading @chromedriver@.
    DownloadError Url HttpException
  | -- | Something went wrong when unzipping @chromedriver@.
    UnzipError ParseError
  deriving (Show)

instance Exception ChromeDriverSetupError
```

The above error type describes different ways setting up `chromedriver` can fail. When we use these
errors, part of that code looks as follows:

```haskell
-- `DownloadError chromeDriverLink` has type `HttpException -> ChromeDriverSetupError`, so when we
-- use it with `mapExceptionM` we will be catching `HttpException`s and turning them into
-- `ChromeDriverSetupError`.
response <- mapExceptionM (DownloadError chromeDriverLink) $ liftIO $ httpLbs request manager
-- `UnzipError` has type `ParseError -> ChromeDriverSetupError`, so when we use it with
-- `mapExceptionM` we will be catching `ParseError`s and turning them into `ChromeDriverSetupError`.
mapExceptionM UnzipError $ unzipIntoPath path $ responseBody response
```

#### `fromX`

The `fromX` functions are useful for turning values into exceptions when one wants to just bubble up
an error value.

##### `fromEither :: (Exception e, MonadThrow m) => Either e a -> m a`

`fromEither` lets us take an ordinary `Either` value and if it's a `Right a`, return `a` in our
monad. If it's a `Left e`, we throw `e`.

##### `fromEitherM :: (Exception e, MonadThrow m) => m (Either e a) -> m a`

This is exactly like `fromEither` but it works when we have an action producing a `Either e a` value.

Let's put this into context with a function using both `fromEither` and `fromEitherM`:

```haskell
data ReadEnvironmentVariableError
  = ReadEnvironmentInvalidValue !EnvironmentKey !EnvironmentValue !String
  | ReadEnvironmentMissingValue !EnvironmentKey
  deriving (Eq, Show)

instance Exception ReadEnvironmentVariableError
```

The usage of `fromEither` and `fromEitherM` is as follows:

```haskell
getEnvironmentValue :: (MonadIO m) => EnvironmentKey -> m (Either EnvironmentKey String)
getEnvironmentValue key = do
  maybeEnvironmentValue <- liftIO $ lookupEnv $ _unEnvironmentKey key
  pure $ maybe (Left key) Right maybeEnvironmentValue

readEnvironmentVariable :: (FromEnvironmentValue a, MonadUnliftIO m) => EnvironmentKey -> m a
readEnvironmentVariable key = do
  -- `getEnvironmentValue key` returns an `Either EnvironmentKey String` value. We use `mapLeft` to
  -- turn any `Left` that might happen into a `ReadEnvironmentMissingValue` error, then throw it. If
  -- the result was actually a `Right` we'll have moved through that section skipping all of that
  -- and `value` will be of type `String`.
  value <- fromEitherM $ mapLeft ReadEnvironmentMissingValue <$> getEnvironmentValue key

  -- `fromEnvironmentValue :: String -> Either String a`
  -- With this function we'll either have a value of type `a` or an error describing why we couldn't
  -- turn the value into `a`. If we have an error, we'll call `handleDecodingError key value` on the
  -- `String` and get a `ReadEnvironmentInvalidValue` error out of it. When we give this `Either`
  -- value to `fromEither` it will throw the `ReadEnvironmentVariableError` or just return the `a`
  -- packaged up in our `m` monad.
  fromEither $ mapLeft (handleDecodingError key value) $ fromEnvironmentValue value
  where
    -- handleDecodingError :: EnvironmentKey -> String -> String -> ReadEnvironmentVariableError
    handleDecodingError k v = ReadEnvironmentInvalidValue k (EnvironmentValue v)
```

##### `fromMaybeM :: (Exception e, MonadThrow m) => e -> m (Maybe a) -> m a`

This is the same as `fromEitherM` but we provide our exception type and if the `Maybe a` value is
`Nothing`, we throw the exception. Otherwise we'll continue as usual:

```haskell
-- `searchPathForBinary :: (MonadUnliftIO m) => FilePath -> m (Maybe FilePath)`
-- Since the result of calling `searchPathForBinary "google-chrome"` is a `Maybe FilePath`, we'll
-- either exit early here with a `ChromeBinaryNotFound` error or we'll continue and `chromeBinary`
-- will be of type `FilePath`, so it can then be used very easily with `getChromeVersion`.
chromeBinary <- fromMaybeM ChromeBinaryNotFound $ searchPathForBinary "google-chrome"
chromeVersion <- getChromeVersion chromeBinary
```

#### `catch`

`catch` and its friends are useful when we believe we can completely recover from an error and
provide a value of the same type as the code that threw the exception.

##### `catch :: (MonadUnliftIO m, Exception e) => m a -> (e -> m a) -> m a`

We can see in the type signature here that our code handling the exception will return `m a`, which
means it's effectively supposed to be a recovery mechanism for code throwing exceptions.

Very commonly you'll see `catch` used in the following way:

```haskell
configuration <- getConfigurationFromFile path `catch` \(e :: ConfigurationFileReadError) -> do
  pure defaultConfiguration
```

We see here that we're using `catch` in the infix position, because it takes the handler as the
second argument. This makes it a very natural candidate for this "hanging" handler style, where the
code doing the recovery hangs off of the end. If your recovery code is complex or at least more
complex than the code you're catching, this is a good way to use it.

Note that we're specifying a specific error type here. If we didn't, the exception catching
machinery wouldn't know which exceptions are to be caught.

A reasonable way to go about this would also be to have the handler defined elsewhere:

```haskell
configuration <- getConfigurationFromFile path `catch` handleConfigurationFileReadError
...
  where
    handleConfigurationFileReadError (ConfigurationFileReadError path) = do
      pure defaultConfiguration
```

##### `catchIO` & `catchAny`

These are specialized versions of `catch` that catch `IOException`s and `SomeException`s,
respectively.

##### `handle`, `handleIO` and `handleAny`

These are really just versions of `catch` that takes the handler as the first argument. The use case
for these is mostly obvious. It's good to know about them if the handling code fits this pattern
better.

##### `catches`

This is a version of `catch` that takes a list of handlers for different exceptions. The handlers
are all of a `Handler` type:

```haskell
configuration <-
  getConfigurationFromFile path
    `catches` [ Handler $ \(e :: ConfigurationParsingError) -> do
                  pure defaultConfiguration,
                Handler $ \(e :: IOException) -> do
                  pure defaultConfiguration
              ]
```

Here we're catching two different exceptions. They happen to have exactly the same handling code in
this case, but as long as both handlers return the same `m a` as the action we are catching
exceptions from, we're fine.

#### `bracket`

When we want to acquire a resource and release it when we're done, we can use `bracket`. This
function takes an action that gives us the resource ("acquire"), a resource that gives back the
resource ("release") as well an action to run on the acquired resource. If an exception occurs in
our action, we'll still run the cleanup/release code.

An example:

```haskell
-- | Executes an action with a session. If an exception is thrown along the way, the session is
-- automatically checked in again.
withSession ::
  (MonadUnliftIO m, MonadThrow m) =>
  TBMQueue SeleniumProcess ->
  (SeleniumProcess -> m a) ->
  m a
withSession sessions action = do
  bracket (checkOutSession sessions) (checkInSession sessions) action
```

In this case we are acquiring a session to use, running the `action` (that takes a
`SeleniumProcess`/session) and when we're done we're checking the session into the queue again.
If we were to throw an exception along the way, the session would still be checked in.

### Reading more

If one wants to read more, there is a
[concise article](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/) that
details some of the considerations that go into handling errors in Haskell.

This article also contains a link to
[a more comprehensive article](https://www.fpcomplete.com/haskell/tutorial/exceptions/) that talks
about these things and more.

### Additions to be made in the future

It's very likely that the concept of asynchronous exceptions will be added to this document in time.
Since this is a tricker concept than bog standard synchronous exception handling and we can manage
exceptions reasonably well using the tools outlined in this document, I think it can be delayed
somewhat.
