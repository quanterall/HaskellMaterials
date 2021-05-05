# JSON data in Haskell

Note: It might be useful to read the [document on type classes](../basics/04-type-classes.md) since
`Aeson` relies on type classes to implement JSON encoding and decoding.

One of the basic things we have to solve when we start using a language (usually) is JSON decoding
and oftentimes JSON encoding. By far the most common solution to this in Haskell is the library
[aeson](https://hackage.haskell.org/package/aeson).

## The basics of `Aeson`

`Aeson` has two basic conceptual components; decoding and encoding.

## Decoding

Decoding is done via the `parseJSON` function in the `FromJSON` type class:

```haskell
class FromJSON a where
  JSON.parseJSON :: JSON.Value -> Data.Aeson.Types.Parser a
  default JSON.parseJSON :: (GHC.Generics.Generic a,
                             JSON.GFromJSON JSON.Zero (GHC.Generics.Rep a)) =>
                             JSON.Value ->
                             Data.Aeson.Types.Parser a
  JSON.parseJSONList :: JSON.Value -> Data.Aeson.Types.Parser [a]
```

We will try to tackle the `Generic` bit here in an extension to this or another text. The `parseJSON`
function that we need to implement in order to have JSON decoding for our type is taking a `Value`,
which is a JSON type that represents any kind of JSON value. This means that we have to essentially
specify how to take a parsed JSON value and create our type from it.

The `Parser` type is essentially just a result type that represents success or failure.

### Defining a `FromJSON` instance

```haskell
newtype Notification = Notification Text

data TemperatureReading = TemperatureReading
  { _sensorId :: SensorId,
    _temperature :: Double,
    _timestamp :: Time.UTCTime,
    _lastReportTime :: Maybe Time.UTCTime,
    _notifications :: [Notification]
  }

instance FromJSON TemperatureReading where
  -- `Parser` is a monad so we can use `do`-notation here. If any of these parses fail the entire
  -- parsing will fail with an error message which will be reflected in the return value,
  -- effectively short-circuiting the entire computation.
  parseJSON = Aeson.withObject "Temperature" $ \parsedObject -> do
    sensorId <- parsedObject .: "sensorId"
    temperature <- parsedObject .: "temperature"
    timestamp <- parsedObject .: "timestamp"
    -- This parses into a `Maybe`; the field is optional.
    lastReportTime <- parsedObject .:? "lastReportTime"
    -- This combination first reads into an optional but then provides a default value.
    -- This is necessary in this case because the type of `_notifications` is `[Notification]`,
    -- not `Maybe [Notification]`
    notifications <- parsedObject .:? "notifications" .!= []

    pure $
      TemperatureReading
        { _sensorId = sensorId,
          _temperature = temperature,
          _timestamp = timestamp,
          _lastReportTime = lastReportTime,
          _notifications = notifications
        }
```

## Encoding

Encoding is done via the `toJSON` function in the `ToJSON` type class:

```haskell
class ToJSON a where
  toJSON :: a -> Value
  default toJSON :: (GHC.Generics.Generic a,
                     Data.Aeson.Types.ToJSON.GToJSON Value Zero (GHC.Generics.Rep a)) =>
                    a -> Value
  toEncoding :: a -> Encoding
  toJSONList :: [a] -> Value
  toEncodingList :: [a] -> Encoding
```

We only need to define the `toJSON` function in order to satisfy this constraint.

### Defining a `ToJSON` instance

If we have the same `Temperature` data type as above, we can do the following to define how to turn
it into `JSON`.

```haskell
instance ToJSON Temperature where
  toJSON Temperature {_sensorId = sensorId, _temperature = temperature, _timestamp = timestamp} =
    JSON.object
      -- This `.=` operator comes from `Data.Aeson` and creates a pair of `Text` & `Value` from the
      -- string supplied on the left and any value on the right with a `ToJSON` instance.
      [ "sensorId" .= sensorId,
        "temperature" .= temperature,
        "timestamp" .= timestamp
        "lastReportTime" .= lastReportTime,
        "notifications" .= notifications
      ]
```

## Automatically generated decoders and encoders

It's quite common to use the type system machinery of the Haskell compiler to automatically generate
JSON decoders and encoders:

```haskell
module Library where

-- requires the package `aeson`
import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
  )
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
-- requires the package `rio`
import RIO
import RIO.Time (UTCTime, getCurrentTime)
import System.IO (print, putStrLn)

newtype Notification = Notification Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype SensorId = SensorId Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data TemperatureReading = TemperatureReading
  { _sensorId :: SensorId,
    _temperature :: Double,
    _timestamp :: UTCTime,
    _lastReportTime :: Maybe UTCTime,
    _notifications :: [Notification]
  }
  deriving (Eq, Show, Generic)

genericJSONOptions :: Options
genericJSONOptions =
  -- Note here that the `fieldLabelModifier` applies to encoding and decoding each field in the
  -- structure, so for each field we are dropping the underscore that the structure's fields contain.
  -- The `fieldLabelModifier` function can be any string modification you want, as we'll illustrate
  -- in an additional example.
  let dropUnderscore :: String -> String
      dropUnderscore = drop 1
   in JSON.defaultOptions {fieldLabelModifier = dropUnderscore}

instance ToJSON TemperatureReading where
  toJSON = JSON.genericToJSON genericJSONOptions

instance FromJSON TemperatureReading where
  parseJSON = JSON.genericParseJSON genericJSONOptions

main :: IO ()
main = do
  now <- getCurrentTime
  let exampleReading =
        TemperatureReading
          { _sensorId = SensorId 42,
            _temperature = 42,
            _timestamp = now,
            _lastReportTime = Just now,
            _notifications = [Notification "Notification example"]
          }
      encodedBytes = JSON.encode exampleReading
      exampleBytes =
        "{\"lastReportTime\":\"2021-05-05T07:26:45.2681256Z\",\"temperature\":42,\"sensorId\":42,\"timestamp\":\"2021-05-05T07:26:45.2681256Z\",\"notifications\":[\"Notification example\"]}"
  print encodedBytes
  case JSON.eitherDecode exampleBytes of
    Right reading@TemperatureReading {} ->
      putStrLn $ "Able to decode: " <> show reading
    Left errorString ->
      putStrLn $ "Unable to decode: " <> show errorString
```

Running the above program will result in the following:

```bash
> stack run
"{\"lastReportTime\":\"2021-05-05T07:26:45.2681256Z\",\"temperature\":42,\"sensorId\":42,
\"timestamp\":\"2021-05-05T07:26:45.2681256Z\",\"notifications\":[\"Notification example\"]}"
Able to decode: TemperatureReading {_sensorId = SensorId 42, _temperature = 42.0,
_timestamp = 2021-05-05 07:26:45.2681256 UTC, _lastReportTime = Just 2021-05-05 07:26:45.2681256 UTC,
_notifications = [Notification "Notification example"]}
```

Note how we (predictably) have removed the underscore from each field in our encoding and we are
able to decode a structure **without** leading underscores. In our internal representation we still
have the underscore.

### More involved field modifications?

What if we have more involved field modifications we need to do? We'll take a look at an example
where we've prefixed the fields with their structure name:

```haskell
module Library where

-- requires the package `aeson`
import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
  )
import qualified Data.Aeson as JSON
import qualified Data.Char as Char
import GHC.Generics (Generic)
-- requires the package `rio`
import RIO
import RIO.Time (UTCTime, getCurrentTime)
import System.IO (print, putStrLn)

newtype Notification = Notification Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype SensorId = SensorId Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data TemperatureReading = TemperatureReading
  { temperatureReadingSensorId :: SensorId,
    temperatureReadingTemperature :: Double,
    temperatureReadingTimestamp :: UTCTime,
    temperatureReadingLastReportTime :: Maybe UTCTime,
    temperatureReadingNotifications :: [Notification]
  }
  deriving (Eq, Show, Generic)

genericJSONOptions :: String -> Options
genericJSONOptions name =
  -- In this case we've elected to do a couple of transformations on the field names, both dropping
  -- the length of a given string as well as lowercasing the first character of the remaining string
  let dropName :: String -> String -> String
      dropName = length >>> drop
      lowerCaseFirstCharacter [] = []
      lowerCaseFirstCharacter (firstCharacter : rest) = Char.toLower firstCharacter : rest
   in JSON.defaultOptions {fieldLabelModifier = dropName name >>> lowerCaseFirstCharacter}

instance ToJSON TemperatureReading where
  toJSON = JSON.genericToJSON $ genericJSONOptions "temperatureReading"

instance FromJSON TemperatureReading where
  parseJSON = JSON.genericParseJSON $ genericJSONOptions "temperatureReading"

main :: IO ()
main = do
  now <- getCurrentTime
  let exampleReading =
        TemperatureReading
          { temperatureReadingSensorId = SensorId 42,
            temperatureReadingTemperature = 42,
            temperatureReadingTimestamp = now,
            temperatureReadingLastReportTime = Just now,
            temperatureReadingNotifications = [Notification "Notification example"]
          }
      encodedBytes = JSON.encode exampleReading
      exampleBytes =
        "{\"lastReportTime\":\"2021-05-05T07:26:45.2681256Z\",\"temperature\":42,\"sensorId\":42,\"timestamp\":\"2021-05-05T07:26:45.2681256Z\",\"notifications\":[\"Notification example\"]}"
  print encodedBytes
  case JSON.eitherDecode exampleBytes of
    Right reading@TemperatureReading {} ->
      putStrLn $ "Able to decode: " <> show reading
    Left errorString ->
      putStrLn $ "Unable to decode: " <> show errorString
```

If we run this program we get this output:

```bash
> stack run
"{\"lastReportTime\":\"2021-05-05T07:46:09.5410336Z\",\"temperature\":42,\"sensorId\":42,
\"timestamp\":\"2021-05-05T07:46:09.5410336Z\",\"notifications\":[\"Notification example\"]}"
Able to decode: TemperatureReading {temperatureReadingSensorId = SensorId 42,
temperatureReadingTemperature = 42.0, temperatureReadingTimestamp = 2021-05-05 07:26:45.2681256 UTC,
temperatureReadingLastReportTime = Just 2021-05-05 07:26:45.2681256 UTC,
temperatureReadingNotifications = [Notification "Notification example"]}
```

Since our `fieldLabelModifier` function can be any function with the type `String -> String` it's
quite possible to do much more advanced things than the ones listed here, but for the purposes of
learning about automatically generated encoders and decoders we won't delve too deep into those.

There are more options you can pass to `genericParseJSON` & `genericToJSON` and it's worth looking
into in order to see what transformations you can apply to your data types, both records and unions.
