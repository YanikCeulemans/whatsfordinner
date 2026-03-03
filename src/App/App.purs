module App.App where

import Prelude

import Data.Date (Date, Month(..))
import Data.Date as Date
import Data.Enum (toEnum)
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Domain.MealSchedule (Id(..), MealSchedule(..))
import Domain.RingList as RingList
import Flame (Application, Html, Update)
import Flame.Html.Element (text)
import Partial.Unsafe (unsafeCrashWith)

type Model = MealSchedule

theDate :: Date
theDate =
  Maybe.fromMaybe' (\_ -> unsafeCrashWith "invalid date literal") $
    Date.canonicalDate
      <$> toEnum 2026
      <*> pure March
      <*> toEnum 2

init :: Model
init =
  MkMealSchedule
    { id: MkId 1
    , startDate: theDate
    , schedule: RingList.fromFoldable []
    }

data Message = NoOp

update :: Update Model Message
update model msg = Tuple model []

view :: Model -> Html Message
view model = text "the view"

app :: Application Model Message
app =
  { subscribe: []
  , view
  , model: init
  , update
  }

