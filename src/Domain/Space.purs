module Domain.Space where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Common as CodecCommon
import Data.Codec.Argonaut.Record as CodecRecord
import Data.String.NonEmpty (NonEmptyString)
import Domain.GroceryListId (GroceryListId)
import Domain.Id as Id
import Domain.MealScheduleId (MealScheduleId)
import Domain.MealScheduleId as MealScheduleId
import Domain.SpaceId (SpaceId)

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
