# Streaming

- [Streaming](#streaming)
  - [`Conduit`](#conduit)
  - [Example pipeline](#example-pipeline)

Sometimes we want to stream values and are unable to accomplish this with lists or other data
structures. For these situations we want to use something more explicitly made for streaming.

## `Conduit`

One of the primary libraries that we can use for streaming in Haskell is called `conduit`, the other
being `pipes`. In this document we will focus on `conduit`, but feel free to explore the differences
between them on your own. You may find that one of them provides a design that matches your mental
model better.

## Example pipeline

```haskell
{-# LANGUAGE DeriveAnyClass #-}

module Library where

import Conduit
import Data.CSV.Conduit
import Data.CSV.Conduit.Conversion
import qualified Data.Conduit.Tar as Tar
import qualified Data.Conduit.Zlib as Zlib
import RIO

data Person = Person
  { name :: Text,
    age :: Maybe Int,
    profession :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromRecord, ToRecord, FromNamedRecord, ToNamedRecord)

newtype Tarball = Tarball {unTarball :: FilePath}
  deriving (Eq, Show)

runMain :: IO ()
runMain = do
  runConduitRes $
    streamFromTarball csvAsPeople (Tar.headerFilePath >>> (== "people.csv")) (Tarball "people.tgz")
      .| printC

-- | Searches a tarball for files matching a predicate, running the supplied conduit on them when
-- found.
streamFromTarball ::
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  -- | Our inner conduit to run on the found CSV file. Note that whatever this conduit does
  -- determines the output of the entire conduit.
  ConduitT ByteString o m () ->
  -- | The predicate that a file in the tarball should match to have the inner conduit run on it.
  (Tar.Header -> Bool) ->
  -- | The tarball to open and search for files in.
  Tarball ->
  ConduitT () o m ()
streamFromTarball innerConduit predicate (Tarball tarballPath) = do
  let matchFile header = when (predicate header) innerConduit
  sourceFileBS tarballPath .| Zlib.ungzip .| Tar.untarChunks .| Tar.withEntries matchFile

csvAsPeople :: (MonadThrow m) => ConduitT ByteString Person m ()
csvAsPeople =
  -- We use `getNamed` here because `Named a` automatically wraps results in a `Named` wrapper, so
  -- in order to just have our type, we need to extract them from that structure.
  intoCSV defCSVSettings .| mapC getNamed
```
