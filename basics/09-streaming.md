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

newtype TarballPath = TarballPath {unTarballPath :: FilePath}
  deriving (Eq, Show)

newtype SoughtFile = SoughtFile {unSoughtFile :: FilePath}
  deriving (Eq, Show)

runMain :: IO ()
runMain = do
  runConduitRes $
    streamFileFromTarball decodeCSVAsPeople (TarballPath "people.tgz") (SoughtFile "people.csv")
      .| printC

-- | Searches a tarball for a given file, running the supplied conduit on it when found.
streamFileFromTarball ::
  forall o m.
  (MonadResource m, PrimMonad m, MonadThrow m) =>
  -- | Our inner conduit to run on the found CSV file. Note that whatever this conduit does
  -- determines the output of the entire conduit.
  ConduitT ByteString o m () ->
  -- | The tarball to open and search for files in.
  TarballPath ->
  -- | The file we are looking for inside of the tarball.
  SoughtFile ->
  ConduitT () o m ()
streamFileFromTarball innerConduit (TarballPath tarballPath) (SoughtFile soughtFile) = do
  let matchFile :: Tar.Header -> ConduitT ByteString o m ()
      matchFile header =
        when (Tar.headerFilePath header == soughtFile) innerConduit
  sourceFileBS tarballPath .| Zlib.ungzip .| Tar.untarChunks .| Tar.withEntries matchFile

decodeCSVAsPeople :: (MonadThrow m) => ConduitT ByteString Person m ()
decodeCSVAsPeople = intoCSV defCSVSettings .| mapC getNamed
```
