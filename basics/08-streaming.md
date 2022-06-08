# Streaming

- [Streaming](#streaming)
  - [`Conduit`](#conduit)
  - [Example pipeline](#example-pipeline)
  - [`ConduitT`](#conduitt)
    - [input](#input)
    - [output](#output)
    - [monad](#monad)
    - [result](#result)
  - [Common functions](#common-functions)
    - [`runConduit`](#runconduit)
    - [`runConduitRes`](#runconduitres)
    - [`yieldMany`](#yieldmany)
    - [`sourceFile{,BS}`](#sourcefile{bs})
    - [Functions similar to non-streaming equivalents](#functions-similar-to-non-streaming-equivalents)

Sometimes we want to stream values and are unable to accomplish this with lists or other data
structures. For these situations we want to use something more explicitly made for streaming. In
general when we want to do something effectful with a collection of values and collect the results,
we'll want streaming behavior. As long as a problem is appropriate for it they also provide a way
of solving issues that is reminiscent of the pipelines we saw in chapter 1.

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

If we were to use `sinkList` in our example pipeline, for example, we'd be able to get all the
people in a list:

```haskell
-- `sinkList` in this pipeline has the type `Monad m => ConduitT i o m [i]`. This means that it will
-- take whatever the input type at the point in the pipeline it's called is and create a list of
-- that type as a result.

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

## Common functions

### `runConduit`

As we've seen above in several examples, `runConduit` takes a pipeline and runs it. It's one of a
few ways to take a pipeline and actually get an end result from it. The resulting value will be of
type `r` in `ConduitT i o m r`.

### `runConduitRes`

This is a variant of `runConduit` that allows us to run `ResourceT` based pipelines. This is needed
when we are allocating some kind of resource in our pipeline, like a file handle or a database
handle. `ResourceT` can then take that allocated resource and automatically free it correctly when
we're done with it.

### `yieldMany`

[`yieldMany`](https://www.stackage.org/haddock/lts-19.10/conduit-1.3.4.2/Conduit.html#v:yieldMany)
takes a collection of things and yields each member individually into the stream. If we have a
list or `Vector`, for example, we can `yieldMany` the contents.

```haskell
scrapeAPIResultForUrls ::
  (MonadUnliftIO m, MonadReader env m, HasHttpManager env, HasLogFunc env) =>
  SourceType ->
  [Url] ->
  m APIResultCounts
scrapeAPIResultForUrls source urls = do
  let emptyCounts =
        APIResultCounts
          { _apiResultCountsGood = 0,
            _apiResultCountsDecodingErrors = 0,
            _apiResultCountsNullResponses = 0,
            _apiResultCountsBadResponseType = 0,
            _apiResultCountsBadResponseStatus = 0
          }
  runConduitRes $
    -- Yield our URLs into the stream
    yieldMany urls
      -- Execute an API call for each
      .| mapMC (getScrapeAPIResult 10)
      -- Reduce/fold the results into a single result structure; `APIResultCounts`
      .| foldlC addCount emptyCounts
  where
    addCount :: APIResultCounts -> Either APIError ScrapeAPIResult -> APIResultCounts
    addCount counts (Right payload)
      | contentType (payload ^. scrapeAPIResultContents) /= expectedResponseType source =
        counts & apiResultCountsBadResponseType +~ 1
      | otherwise = counts & apiResultCountsGood +~ 1
    addCount counts (Left WrongPayload) = counts & apiResultCountsDecodingErrors +~ 1
    addCount counts (Left NullResponse) = counts & apiResultCountsNullResponses +~ 1
    addCount counts (Left BadStatus) = counts & apiResultCountsBadResponseStatus +~ 1
```

### `sourceFile{,BS}`

[`sourceFile`](https://www.stackage.org/haddock/lts-19.10/conduit-1.3.4.2/Conduit.html#v:sourceFile)
takes a file path and establishes a stream of `ByteString` values from that file.

```haskell
-- Reads a file, unpacking it as a gzip archive and reads tarball entries from it
sourceFileBS tarballPath .| unTarGz .| Tar.withEntries matchFile
```

### Functions similar to non-streaming equivalents

All the below functions work exactly as you would expect if you know their non-streaming analogues:

[`mapC`](https://www.stackage.org/haddock/lts-19.10/conduit-1.3.4.2/Conduit.html#v:mapC), like its
non-streaming equivalent, maps a function over the stream, modifying each value before it is passed
on to the next step.

[`filterC`](https://www.stackage.org/haddock/lts-19.10/conduit-1.3.4.2/Conduit.html#v:filterC)
passes only values matching a given predicate to the next step in the pipeline.

[`takeC`](https://www.stackage.org/haddock/lts-19.10/conduit-1.3.4.2/Conduit.html#v:takeC) takes a
given number of elements from the stream.

[`takeWhile`](https://www.stackage.org/haddock/lts-19.10/conduit-1.3.4.2/Conduit.html#v:takeWhileC)
takes values from the stream and passes them on until it finds one that does not match the
predicate.

[`concatC`](https://www.stackage.org/haddock/lts-19.10/conduit-1.3.4.2/Conduit.html#v:concatC)
takes a stream of foldables and flattens them out into streams of each separate item.
