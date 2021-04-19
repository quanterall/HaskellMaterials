# The ReaderT Monad Transformer

## Monad transformers

A monad transformer is a data type that allows us to wrap one monad in another. What this means is
that it enables a context wherein we get the capabilities of both the transformer and the monad it
is wrapping. Monad transformers are themselves monads, which means we can create multiple layers to
get an arbitrary amount of capabilities. It's relatively rare in normal applications that you'd
create big stacks of monad transformers, however, as most things can be done with `ReaderT` wrapping
the `IO` monad.

## ReaderT

```haskell
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}
```

The definition here itself isn't as important initially as the type signature and what the different
parts represent. We can restate the definition as follows:

```haskell
newtype ReaderT environment wrappedMonad returnValue =
  ReaderT {runReaderT :: environment -> wrappedMonad returnValue}
```

The `wrappedMonad` type will decide what we can **additionally** do, on top of having implicit
values we can read from an environment. It's very common to create applications that execute in the
`ReaderT appState IO a` context, where `appState` is the application's application state. This
allows for implicit environment reading as well as `IO` actions, which means we can read from
`IORef`s, use transactional variables and so on. This makes it so that we get access to a very
tightly controlled usage of mutable variables with a clear initialization stage, as the application
state has to be initialized with `runReaderT` at the start of the application.

## Small example

```haskell
data ApplicationState = ApplicationState
  { databaseConnection :: DBConnection,
    logHandle :: LogHandle
  }

data DeletionResult
  = NotFound
  | AssociationError
  | Deleted

-- Execution starts here
main :: IO ()
main = do
  -- We set up the initial application state so that `runApplication` and everything it runs has
  -- access to it.
  databaseInfo <- Environment.get "DB_CONNECTION_INFO"
  dbConnection <- connectToDatabase databaseInfo
  logHandle' <- setupLog
  let applicationState =
        ApplicationState
          { databaseConnection = dbConnection,
            logHandle = logHandle'
          }

  runReaderT runApplication applicationState

runApplication :: ReaderT ApplicationState IO ()
runApplication = do
  -- Implementation of request handlers here, perhaps for a web server.
  -- One of them will use `handleDeleteUserRequest`, which has access to the already set up
  -- environment.
  undefined

handleDeleteUserRequest :: UserId -> ReaderT ApplicationState IO ()
handleDeleteUserRequest userId = do
  deletionResult <- runDatabase $ deleteUserWithId userId
  case deletionResult of
    Deleted -> do
      logInfo $ "Found and deleted user with ID: " <> show userId
      
    NotFound ->
      logWarn $ "Unable to find user with ID: " <> show userId <> " for deletion."

    AssociationError ->
      logError $
        mconcat
          [ "Unable to delete user with ID '",
            show userId,
            "' because stuff relies on it existing."
          ]

logInfo :: Text -> ReaderT ApplicationState IO ()
logInfo message = logMessage "INFO" message

logWarn :: Text -> ReaderT ApplicationState IO ()
logWarn message = logMessage "WARN" message

logError :: Text -> ReaderT ApplicationState IO ()
logError message = logMessage "ERROR" message

logMessage :: Text -> Text -> ReaderT ApplicationState IO ()
logMessage prefix message = do
  handle <- asks logHandle
  -- This hypothetical function requires `IO`, so we lift the context into the wrapped `IO` we have
  liftIO $ writeToLogHandle handle $ "[" <> prefix <> "]" <> message

runDatabase :: DatabaseAction a -> ReaderT ApplicationState IO a
runDatabase action = do
  connection <- asks databaseConnection
  -- This hypothetical function requires `IO`, so we lift the context into the wrapped `IO` we have
  liftIO $ runQuery connection action

deleteUserWithId :: UserId -> DatabaseAction DeletionResult
deleteUserWithId userId = deleteEntity userId
```

## Bigger example

TODO: add proper motivating example (ironically easier than for just `Reader`)
