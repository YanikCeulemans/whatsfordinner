module App.App where

import Prelude

import App.Data as AData
import App.Groceries as Groceries
import App.Layout as Layout
import App.Route (Route(..))
import App.Route as Route
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
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowDate)
import FFI.Doc as FFIDoc
import FFI.Navigation as FFINav
import Flame (Application, Html, Update)
import Flame (AppId) as F
import Flame.Application (noMessages) as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Subscription as FS
import Partial.Unsafe (unsafeCrashWith)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget as ET
import Web.HTML (window)
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState(..))
import Web.HTML.Window (document)

type HomeModel =
  { mealSchedule :: MealSchedule
  }

type GroceriesGenerateModel =
  { startDate :: Maybe Date
  , endDate :: Maybe Date
  }

data RouteModel
  = HomeM HomeModel
  | GroceriesM Groceries.Model
  | GroceriesGenerateM GroceriesGenerateModel

type Model =
  { route :: RouteModel
  , targetDate :: Date
  }

routeToModel :: Model -> Maybe Route -> Model
routeToModel model = case _ of
  Just Groceries -> model { route = GroceriesM $ Groceries.init }
  Just GroceriesGenerate -> model
    { route = GroceriesGenerateM { startDate: Nothing, endDate: Nothing } }
  _ -> model
    { route = HomeM { mealSchedule: AData.mealSchedule } }

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

homeView :: Date -> HomeModel -> Html Message
homeView targetDate model =
  let
    weekDays = nextDays 7 targetDate
    nextWeekDate = unsafeAdjustDate (Days 7.0) targetDate
    dateRange = Range.create targetDate nextWeekDate
    weekMeals =
      Array.fromFoldable $ MealSchedule.toList
        dateRange
        model.mealSchedule
    zipped = Array.zip weekDays weekMeals
  in
    Layout.main $
      HE.div [ HA.class' "flex column" ]
        [ HE.h2_ [ HE.text "Yolo" ]
        , case zipped of
            [] -> HE.text ""
            entries ->
              HE.div [ HA.class' "flex column spaced" ]
                $ map (viewScheduleEntry targetDate) entries
        ]

-- [ HE.h1_ [ HE.text "The next 7 days" ]
-- , case zipped of
--     [] -> HE.text "empty"
--     entries ->
-- -- HE.div [ HA.class' "flex column spaced" ]
-- --   $ map (viewScheduleEntry targetDate) entries
-- ]

view :: Model -> Html Message
view model = case model.route of
  HomeM homeM -> homeView model.targetDate homeM
  GroceriesM groceriesM -> GroceriesMessage <$> Groceries.view groceriesM
  GroceriesGenerateM _groceriesGenerateM -> Layout.main $ HE.text "TODO"

app :: Date -> String -> Application Model Message
app startDate documentUrlText =
  { subscribe:
      [ FS.onCustomEvent' (EventType "visibilitychange")
          DocumentVisibilityChanged
      ]
  , view
  , model: init documentUrlText startDate
  , update
  }

