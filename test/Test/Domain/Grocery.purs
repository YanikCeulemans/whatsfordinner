module Test.Domain.Grocery (spec) where

import Prelude

import Data.Argonaut as J
import Data.Array (fold)
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Data.Either as Either
import Data.ULID as DULID
import Domain.Amount as Amount
import Domain.Grocery (Grocery)
import Domain.Grocery as Grocery
import Domain.GroceryId (GroceryId)
import Domain.GroceryId as GroceryId
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual)

rawGroceryId :: String
rawGroceryId = "01KNEQ7KMSBM0Q4XP56C6YP3NG"

groceryId :: GroceryId
groceryId = GroceryId.MkGroceryId
  $ Either.fromRight' crash
  $ DULID.parse rawGroceryId
  where
  crash _ = unsafeCrashWith "invalid hardcoded ulid"

quoted :: String -> String
quoted x = fold [ "\"", x, "\"" ]

encode :: Grocery -> J.Json
encode = CA.encode Grocery.codec

decode :: J.Json -> Either CA.JsonDecodeError Grocery
decode = CA.decode Grocery.codec

parseJson :: String -> J.Json
parseJson = J.parseJson >>> Either.fromRight' crash
  where
  crash _ = unsafeCrashWith "invalid hardcoded json"

grocery :: Grocery
grocery = Grocery.create groceryId "Tomatoes" $ Amount.unitless 1.0

codecSpec :: Spec Unit
codecSpec =
  describe "codec" do
    it "serialisation works for grocery" do
      let
        actual = J.stringify $ encode grocery

      actual `shouldEqual`
        ( fold
            [ """{"amount":{"value":1},"checked":false,"description":"Tomatoes","id":"""
            , quoted rawGroceryId
            , "}"
            ]
        )

    it "deserialization works for grocery" do
      let
        actual = decode $ parseJson
          ( fold
              [ """{"amount":{"value":1},"checked":false,"description":"Tomatoes","id":"""
              , quoted rawGroceryId
              , "}"
              ]
          )
      actual `shouldContain` grocery

spec :: Spec Unit
spec =
  describe "Grocery" do
    codecSpec

