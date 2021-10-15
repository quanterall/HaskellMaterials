# Streaming

- [Streaming](#streaming)
  - [`Conduit`](#conduit)
  - [Example pipeline](#example-pipeline)
  - [`ConduitT`](#conduitt)
    - [input](#input)
    - [output](#output)
    - [monad](#monad)
    - [result](#result)

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
  -- The "Record" derivations here are for CSV decoding. We automatically get CSV decoding for free
  -- if we derive these.
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
  sourceFileBS tarballPath .| unTarGz .| Tar.withEntries matchFile

csvAsPeople :: (MonadThrow m) => ConduitT ByteString Person m ()
csvAsPeople =
  -- We use `getNamed` here because `Named a` automatically wraps results in a `Named` wrapper, so
  -- in order to just have our type, we need to extract them from that structure.
  intoCSV defCSVSettings .| mapC getNamed

-- | Untars and ungzips a `ByteStream`, akin to extracting the raw data from a `.tar.gz` file.
unTarGz :: (PrimMonad m, MonadThrow m) => ConduitT ByteString Tar.TarChunk m ()
unTarGz = Zlib.ungzip .| Tar.untarChunks
```

## `ConduitT`

`conduit` is centered around the `ConduitT` type:

```haskell
data ConduitT input output monad result
```

These type variables are expanded from their usual forms temporarily to make it more obvious what is
what; they are usually just `i`, `o`, `m` and `r`.

### input

Each conduit has a possible input type. This is a **per element** type. A conduit with an input type
of `ByteString` will take byte strings as input, i.e. each thing it processes will be a byte string.
This becomes more obvious when we talk about something that is not necessarily reducible into
component types, like `Int`. If a conduit takes `Int` as input, it means it processes `Int`s.

### output

Likewise, we also have an output type. This is the kind of element the conduit passes "downstream",
i.e. to the next conduit.

### monad

`conduit` is (in part) interesting because it allows us to make each component of the pipeline do
whatever it needs to in order to get its data. This means that we might want to execute in special
contexts for the components. The `m` part here is what allows us to grant a pipeline these extra
capabilities via special contexts. Having `IO` as your `monad` type in a `ConduitT`, for example,
will allow you to read files, and so on.

### result

You might be wondering what the difference between `output` and `result` is. `output` is for passing
downstream and `result` is for final results. The expectation here is that there needs to be
something that takes care of whatever output you are passing downstream, whereas a `result` is for
a final result in the pipeline, retrieved when we run `runConduit`/`runConduitRes`.

If we were to use `sinkList` in the middle of our example pipeline, for example, we'd be able to get
all the people in a list:

```haskell
-- sinkList :: Monad m => ConduitT i o m [i]

runMain :: IO ()
runMain = do
  people <-
    runConduitRes $
      streamFromTarball csvAsPeople (Tar.headerFilePath >>> (== "people.csv")) (Tarball "people.tgz")
        .| sinkList
  print people
```

We can see in the type signature of `sinkList` that for any `a` input type it gets, it will assemble
them and return them as a list. It's important to note that this is a **sink**, which means it will
await new elements until there are none left from upstream, then return them. This all also means
that we'll have it at the end of our pipeline.

Most of the components in a pipeline will have a result type of `()`, which means they don't have a
result type; they're just trying to look at whatever input came in, then produce output based on it.
