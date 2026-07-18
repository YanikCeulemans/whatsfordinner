module Common.Space where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Common as CodecCommon
import Data.Codec.Argonaut.Record as CodecRecord
import Data.String.NonEmpty (NonEmptyString)
import Common.GroceryListId (GroceryListId)
import Common.Id as Id
import Common.MealScheduleId (MealScheduleId)
import Common.MealScheduleId as MealScheduleId
import Common.SpaceId (SpaceId)

type Space =
  { id :: SpaceId
  , name :: NonEmptyString
  , mealScheduleId :: MealScheduleId
  , groceryListId :: GroceryListId
  }

spaceCodec :: JsonCodec Space
spaceCodec =
  CodecRecord.object "Space"
    { id: Id.codec
    , name: CodecCommon.nonEmptyString
    , mealScheduleId: MealScheduleId.codec
    , groceryListId: Id.codec
    }
