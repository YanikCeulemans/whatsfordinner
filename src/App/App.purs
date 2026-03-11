module App.App where

import Prelude

import Data.Array as Array
import Data.Date (Date, Month(..), Weekday(..))
import Data.Date as Date
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import Data.Foldable (fold)
import Data.Formatter.DateTime as Format
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Domain.Meal (Meal(..))
import Domain.MealSchedule (Id(..), MealSchedule(..))
import Domain.MealSchedule as MealSchedule
import Domain.PlannedMeal (PlannedMeal(..))
import Domain.Range (Range)
import Domain.Range as Range
import Domain.RingList as RingList
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import FFI.FFIDoc as FFIDoc
import Flame (Application, Html, Update)
import Flame.Application as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Subscription as FS
import Partial.Unsafe (unsafeCrashWith)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState(..))
import Web.HTML.Window (document)

type Model =
  { mealSchedule :: MealSchedule
  , targetDate :: Date
  }

theDate :: Date
theDate =
  Maybe.fromMaybe' (\_ -> unsafeCrashWith "invalid date literal") $
    Date.canonicalDate
      <$> toEnum 2026
      <*> pure March
      <*> toEnum 2

init :: Date -> Model
init targetDate =
  { mealSchedule
  , targetDate
  }
  where
  mealSchedule =
    MkMealSchedule
      { id: MkId 1
      , startDate: theDate
      , schedule: RingList.fromFoldable
          [ PlannedMeal
              ( MkMeal
                  { name: "Brussels sprouts with bacon and mashed potatoes" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Mac & Cheese" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Chili sin carne" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Chicken wok with noodles" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Vol au vent" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Leek, mashed potatoes and minced meat casserole" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Fishsticks with broccoli and baby potatoes" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Scampi diabolique with bread" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Cordon blue with carrots and potatoes" }
              )
          , PlannedMeal
              ( MkMeal
                  { name:
                      "Veggie Snitzel with bell peppers, zucchini and baby potatoes"
                  }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Chicken with curry and pineapple" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Pita" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Chicory ham rolls with mashed potatoes" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Pasta bolognese" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Sausage with carrot mash" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Pasta with broccoli, leek and herbcheese" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Veggie Snitzel with beans and potatoes" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Chicken wrap" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Lasagne" }
              )
          , NoMealPlanned
          , PlannedMeal
              ( MkMeal
                  { name: "Codloin with basilicum mash and vine tomatoes" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Orzo with olives and feta" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Pasta with mushroom cream sauce" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Chicken, apple mash and fried potatoes" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Veggie burger with spinach mash" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Sandwiches" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Zucchini, minced meat and potato slices casserole" }
              )
          , PlannedMeal
              ( MkMeal
                  { name: "Salmon with pesto crust, greens and baby potatoes" }
              )
          ]
      }

data Message
  = DocumentVisibilityChanged
  | TargetDateUpdated Date

update :: Update Model Message
update model = case _ of
  DocumentVisibilityChanged ->
    model /\
      [ do
          vs <- liftEffect $ FFIDoc.visibilityState =<< document =<< window
          case vs of
            Visible -> Just <<< TargetDateUpdated <$> liftEffect nowDate
            _ -> pure Nothing
      ]

  TargetDateUpdated newDate ->
    F.noMessages $ model { targetDate = newDate }

displayDateTime :: DateTime -> String
displayDateTime =
  Format.format $ List.fromFoldable
    [ Format.DayOfMonthTwoDigits
    , Format.Placeholder "/"
    , Format.MonthTwoDigits
    , Format.Placeholder "/"
    , Format.YearFull
    ]

displayDate :: Date -> String
displayDate d = displayDateTime $ DateTime d bottom

startOfWeekDay :: Weekday
startOfWeekDay = Monday

endOfWeekDay :: Weekday
endOfWeekDay = Sunday

startOfWeek :: Date -> Date
startOfWeek d
  | Date.weekday d == startOfWeekDay = d
  | otherwise = startOfWeek
      $ Maybe.fromMaybe' (\_ -> unsafeCrashWith "invalid days amount")
      $ Date.adjust (Days (-1.0)) d

endOfWeek :: Date -> Date
endOfWeek d
  | Date.weekday d == endOfWeekDay = d
  | otherwise = endOfWeek
      $ Maybe.fromMaybe' (\_ -> unsafeCrashWith "invalid days amount")
      $ Date.adjust (Days 1.0) d

unsafeAdjustDate :: Days -> Date -> Date
unsafeAdjustDate d =
  Maybe.fromMaybe' (\_ -> unsafeCrashWith "invalid days amount")
    <<< Date.adjust d

weekRange :: Date -> Range Date
weekRange d = Range.create start end
  where
  start = startOfWeek d
  end = endOfWeek d

nextDays :: Int -> Date -> Array Date
nextDays n date
  | n < 1 = []
  | otherwise =
      unfoldr go n
      where
      go n'
        | n' < 1 = Nothing
        | otherwise = Just $
            unsafeAdjustDate (Days $ Int.toNumber $ n - n') date /\ (n' - 1)

viewScheduleEntry :: Date -> Tuple Date PlannedMeal -> Html Message
viewScheduleEntry date (mealDate /\ plannedMeal) =
  HE.article_
    [ HE.header_
        [ HE.text $ fold
            [ show $ Date.weekday mealDate
            , ": "
            , displayDate mealDate
            , if date == mealDate then " (Today)" else ""
            ]
        ]
    , case plannedMeal of
        NoMealPlanned -> HE.text "No meal planned"
        PlannedMeal meal -> HE.span [] [ HE.text $ show meal ]
    ]

view :: Model -> Html Message
view model =
  let
    weekDays = nextDays 7 model.targetDate
    nextWeekDate = unsafeAdjustDate (Days 7.0) model.targetDate
    dateRange = Range.create model.targetDate nextWeekDate
    weekMeals =
      Array.fromFoldable $ MealSchedule.toList
        dateRange
        model.mealSchedule
    zipped = Array.zip weekDays weekMeals
  in
    HE.main [ HA.class' "flex column spaced container" ]
      [ case zipped of
          [] -> HE.text ""
          entries ->
            HE.div [ HA.class' "flex column spaced" ]
              $ map (viewScheduleEntry model.targetDate) entries
      , HE.h1_ [ HE.text "Entire schedule" ]
      ]

app :: Date -> Application Model Message
app targetDate =
  { subscribe:
      [ FS.onCustomEvent' (EventType "visibilitychange")
          DocumentVisibilityChanged
      ]
  , view
  , model: init targetDate
  , update
  }

