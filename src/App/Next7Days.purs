module App.Next7Days where

import Prelude

import App.Data as AData
import App.Layout as Layout
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
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Domain.MealSchedule (MealSchedule)
import Domain.MealSchedule as MealSchedule
import Domain.PlannedMeal (PlannedMeal(..))
import Domain.Range (Range)
import Domain.Range as Range
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)

type InitializedState =
  { mealSchedule :: MealSchedule
  , date :: Date
  }

data State
  = NotInitialized
  | Initialized InitializedState

data Action = Initialize

{--
init :: String -> Date -> Model
init startUrlText targetDate =
  routeToModel initModel $ Route.parse startUrlText
  where
  initModel = { route: HomeM { mealSchedule: AData.mealSchedule }, targetDate }

handleOnNavigate :: F.AppId String Message -> Event -> Effect Unit
handleOnNavigate appId evt =
  liftEffect $ FFINav.intercept opts navEvt
  where
  navEvt = FFINav.fromEvent evt
  opts =
    { handler: do
        liftEffect $ FS.send appId $ NavigationOccurred
          navEvt.destination.url
    }

initializeApp
  :: forall m. MonadEffect m => F.AppId String Message -> m (Maybe Message)
initializeApp appId = liftEffect do
  onNavigate <- ET.eventListener $ handleOnNavigate appId
  navigation <- FFINav.toEventTarget <$> FFINav.navigation
  ET.addEventListener (EventType "navigate") onNavigate true navigation
  pure Nothing

data Message
  = InitializeRequested (F.AppId String Message)
  | DocumentVisibilityChanged
  | TargetDateUpdated Date
  | NavigationOccurred String
  | GroceriesMessage Groceries.Message

update :: Update Model Message
update model message = case message of
  InitializeRequested appId ->
    model /\ [ initializeApp appId ]
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

  NavigationOccurred destinationUrlText ->
    updatedModel /\ []
    where
    updatedModel = routeToModel model $ Route.parse destinationUrlText

  GroceriesMessage groceriesMessage ->
    case model.route of
      GroceriesM groceriesModel ->
        updatedModel /\ effs
        where
        updatedGroceriesM /\ groceriesEffs = Groceries.update groceriesModel
          groceriesMessage
        updatedModel = model
          { route = GroceriesM updatedGroceriesM }
        effs = (map <<< map <<< map) GroceriesMessage groceriesEffs
      _ -> model /\ []
--}

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

component
  :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render: HH.fromPlainHTML <<< render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize, handleAction = handleAction }
    }

  where
  initialState :: input -> State
  initialState _ = NotInitialized

  handleAction
    :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> do
      now <- H.liftEffect nowDate
      H.modify_ \_ -> Initialized
        { date: now, mealSchedule: AData.mealSchedule }
      pure unit

  render :: State -> HH.PlainHTML
  render state =
    Layout.main $
      case state of
        NotInitialized -> HH.p_ [ HH.text "loading" ]
        Initialized initializedState ->
          HH.div [ HP.class_ $ H.ClassName "flex column" ]
            [ HH.h1_ [ HH.text "The next 7 days" ]
            , case zipped of
                [] -> HH.text ""
                entries ->
                  HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
                    $ map (viewScheduleEntry initializedState.date) entries
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

