module Data.ULID (parse) where

import Prelude

import Data.Array (elem)
import Data.Array as Array
import Data.Either (Either(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Simple.ULID (ULID)
import Unsafe.Coerce (unsafeCoerce)

encoding :: String
encoding = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

parse :: String -> Either String ULID
parse candidate
  | String.length candidate /= 26 = Left "must be 26 chars in length"
  | otherwise =
      if candidateChars # Array.all (_ `elem` encodingChars) # not then
        Left "must only contain characters in base32 encoding"
      else
        Right (unsafeCoerce candidate)
      where
      candidateChars = CodeUnits.toCharArray candidate
      encodingChars = CodeUnits.toCharArray encoding
