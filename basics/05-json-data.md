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
