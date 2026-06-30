module Domain.Extensions.Date (codec) where

import Prelude

import Data.Argonaut as Json
import Data.Array (intersperse)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as Codec
import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Either (Either)
import Data.Either as Either
import Data.Formatter.DateTime
  ( Formatter
  , FormatterCommand(..)
  , format
  , unformat
  )
import Data.List as List

dateFormatter :: Formatter
dateFormatter =
  List.fromFoldable
    $ intersperse (Placeholder "-")
    $ [ YearFull, MonthTwoDigits, DayOfMonthTwoDigits ]

toDateTime :: Date -> DateTime
toDateTime date = DateTime date bottom

parseDate :: String -> Either String Date
parseDate candidate =
  unformat dateFormatter candidate
    <#> DateTime.date

codec :: JsonCodec Date
codec = Codec.codec' deserialize serialize
  where
  deserialize candidate =
    Json.toString candidate
      # Either.note (TypeMismatch "String")
      <#> parseDate
      <#> lmap (const $ TypeMismatch "Date")
      # join
  serialize date =
    Json.fromString $ format dateFormatter $ toDateTime date
