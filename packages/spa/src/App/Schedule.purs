module Spa.App.Schedule where

import Prelude

import Common.GroceryListId (GroceryListId)
import Common.MealSchedule (MealSchedule)
import Common.MealSchedule as MealSchedule
import Common.PlannedMeal (PlannedMeal(..))
import Common.Range (Range)
import Common.Range as Range
import Common.SpaceId (SpaceId)
import Data.Array as Array
import Data.Date (Date, Weekday(..))
import Data.Date (adjust, weekday) as Date
import Data.DateTime (DateTime(..))
import Data.Foldable (fold)
import Data.Formatter.DateTime as Format
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Time.Duration (Days(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Spa.App.Layout as Layout
import Spa.App.RemoteData (RemoteData(..))
import Spa.App.RemoteData as RemoteData
import Spa.Capabilities.Resource.ManageMealSchedule
  ( class ManageMealSchedule
  , loadMealSchedule
  )
import Spa.Capabilities.Resource.ManageSpaces (class ManageSpaces, loadSpace)

type Input = SpaceId

type ScheduleState =
  { groceryListId :: GroceryListId
  , mealSchedule :: MealSchedule
  , date :: Date
  , now :: Date
  }

type State =
  { spaceId :: SpaceId
  , scheduleState :: RemoteData String ScheduleState
  }

data Action
  = Initialize
  | BackInTime
  | ForwardInTime

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

timeTravel :: Days -> State -> State
timeTravel days state =
  state { scheduleState = timeTravel' <$> state.scheduleState }
  where
  timeTravel' s = s { date = thePast s.date }
  thePast date =
    case Date.adjust days date of
      Nothing -> unsafeCrashWith
        "invalid date created while time traveling?!"
      Just pastDate -> pastDate

viewScheduleEntry :: Date -> Tuple Date PlannedMeal -> HH.PlainHTML
viewScheduleEntry date (mealDate /\ plannedMeal) =
  HH.article_
    [ HH.header_
        [ HH.text $ fold
            [ show $ Date.weekday mealDate
            , ": "
            , displayDate mealDate
            , if date == mealDate then " (Today)" else ""
            ]
        ]
    , case plannedMeal of
        NoMealPlanned -> HH.text "No meal planned"
        PlannedMeal meal -> HH.span [] [ HH.text $ show meal ]
    ]

buildMainLayoutConfig :: State -> Layout.MainConfig
buildMainLayoutConfig state =
  Layout.defaultMainConfig { routing = Just routing }
  where
  groceryListId = _.groceryListId <$> RemoteData.toMaybe state.scheduleState
  routing = { spaceId: state.spaceId, groceryListId: groceryListId }

component
  :: forall query output m
   . MonadAff m
  => ManageSpaces m
  => ManageMealSchedule m
  => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render: render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize, handleAction = handleAction }
    }

  where
  initialState :: Input -> State
  initialState spaceId =
    { spaceId
    , scheduleState: NotRequested
    }

  handleAction
    :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> do
      H.modify_ _ { scheduleState = Loading }
      foundSpace <- loadSpace =<< H.gets _.spaceId
      for_ foundSpace \{ groceryListId, mealScheduleId } -> do
        now <- H.liftEffect nowDate
        foundMealSchedule <- loadMealSchedule mealScheduleId
        let
          scheduleState =
            foundMealSchedule
              # RemoteData.note "No such meal schedule exists"
              <#> { date: now, now, groceryListId, mealSchedule: _ }

        H.modify_ _
          { scheduleState = scheduleState }

    BackInTime -> do
      H.modify_ $ timeTravel $ Days $ -3.0

    ForwardInTime -> do
      H.modify_ $ timeTravel $ Days $ 3.0

  render :: State -> H.ComponentHTML Action () m
  render state =
    Layout.main' (buildMainLayoutConfig state) $
      case state.scheduleState of
        NotRequested -> HH.p_ [ HH.text "loading" ]
        Loading -> HH.p_ [ HH.text "loading" ]
        Error e -> HH.p_ [ HH.text e ]
        Success initializedState ->
          HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
            [ HH.h1_ [ HH.text "Schedule" ]
            , HH.div
                [ HP.class_ $ H.ClassName "flex row justify-space-between" ]
                [ HH.button [ HE.onClick (const $ BackInTime) ]
                    [ HH.text "Back in time" ]
                , HH.button [ HE.onClick (const $ ForwardInTime) ]
                    [ HH.text "Forward in time" ]
                ]
            , case zipped of
                [] -> HH.text ""
                entries ->
                  HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
                    $ map
                        ( HH.fromPlainHTML <<< viewScheduleEntry
                            initializedState.now
                        )
                        entries
            ]
          where
          targetDate = initializedState.date
          weekDays = nextDays 7 targetDate
          nextWeekDate = unsafeAdjustDate (Days 7.0) targetDate
          dateRange = Range.create targetDate nextWeekDate
          weekMeals =
            Array.fromFoldable $ MealSchedule.toList
              dateRange
              initializedState.mealSchedule
          zipped = Array.zip weekDays weekMeals
