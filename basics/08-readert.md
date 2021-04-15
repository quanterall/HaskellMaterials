# The ReaderT Monad Transformer

A monad transformer is a data type that allows us to wrap one monad in another. As an example, we
have the `ReaderT` monad transformer:

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
```

## Bigger example

TODO: add proper motivating example (ironically easier than for just `Reader`)
