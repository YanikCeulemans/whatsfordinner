module Domain.Space where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Common as CodecCommon
import Data.Codec.Argonaut.Record as CodecRecord
import Data.String.NonEmpty (NonEmptyString)
import Domain.GroceryList (GroceryList)
import Domain.GroceryList as GroceryList
import Domain.Id as Id
import Domain.MealSchedule (MealSchedule)
import Domain.SpaceId (SpaceId)

type Space =
  { id :: SpaceId
  , name :: NonEmptyString
  , mealSchedule :: MealSchedule
  , groceryList :: GroceryList
  }

spaceCodec :: JsonCodec Space
spaceCodec =
  CodecRecord.object
    { id: Id.codec
    , name: CodecCommon.nonEmptyString
    , mealSchedule: MealSchedule.codec
    , groceryList: GroceryList.codec
    }
