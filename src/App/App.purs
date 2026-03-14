module App.App where

import Prelude

import App.Data as AData
import Data.Array as Array
import Data.Date (Date, Month(..), Weekday(..))
import Data.Date as Date
import Data.DateTime (DateTime(..))
import Data.Either as Either
import Data.Enum (toEnum)
import Data.Foldable (fold)
import Data.Formatter.DateTime as Format
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Debug as Debug
import Domain.MealSchedule (MealSchedule)
import Domain.MealSchedule as MealSchedule
import Domain.PlannedMeal (PlannedMeal(..))
import Domain.Range (Range)
import Domain.Range as Range
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import FFI.Doc as FFIDoc
import FFI.URL as FFIURL
import Flame (Application, Html, Update)
import Flame.Application as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Subscription as FS
import Partial.Unsafe (unsafeCrashWith)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as D
import Routing.Duplex.Generic as G
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState(..))
import Web.HTML.Window (document)

type HomeModel =
  { mealSchedule :: MealSchedule
  }

type GroceriesModel = Unit

data RouteModel
  = HomeM HomeModel
  | GroceriesM GroceriesModel

type Model =
  { route :: RouteModel
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
  { route: HomeM
      { mealSchedule: AData.mealSchedule targetDate
      }
  , targetDate
  }

data Route
  = Home
  | Groceries

derive instance Generic Route _

route :: RouteDuplex' Route
route = D.root $ G.sum
  { "Home": G.noArgs
  , "Groceries": D.path "groceries" G.noArgs
  }

parseRoute :: String -> Maybe Route
parseRoute = D.parse route >>> Either.hush

data Message
  = DocumentVisibilityChanged
  | TargetDateUpdated Date
  | NavigationOccurred String

update :: Update Model Message
update model message = case model.route /\ message of
  _ /\ DocumentVisibilityChanged ->
    model /\
      [ do
          vs <- liftEffect $ FFIDoc.visibilityState =<< document =<< window
          case vs of
            Visible -> Just <<< TargetDateUpdated <$> liftEffect nowDate
            _ -> pure Nothing
      ]

  HomeM _ /\ TargetDateUpdated newDate ->
    F.noMessages $ model { targetDate = newDate }

  _ /\ NavigationOccurred destinationUrlText ->
    updatedModel /\ []
    where
    parsedRoute = do
      url <- Either.hush $ FFIURL.mk destinationUrlText
      let
        pathAndQuery = fold [ FFIURL.pathname url, FFIURL.search url ]
      parseRoute pathAndQuery
    updatedModel = case parsedRoute of
      Just Groceries -> model { route = GroceriesM unit }
      _ -> model
        { route = HomeM { mealSchedule: AData.mealSchedule model.targetDate } }

  _ -> F.noMessages model

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
    HE.fragment
      [ HE.main [ HA.class' "flex column container" ]
          [ HE.h1_ [ HE.text "The next 7 days" ]
          , case zipped of
              [] -> HE.text ""
              entries ->
                HE.div [ HA.class' "flex column spaced" ]
                  $ map (viewScheduleEntry targetDate) entries
          ]
      , HE.footer [ HA.class' "container" ]
          [ HE.nav [ HA.class' "flex spaced justify-center" ]
              [ HE.a
                  [ HA.href "/" ]
                  [ HE.text "Next days" ]
              , HE.a
                  [ HA.href "/groceries" ]
                  [ HE.text "Groceries" ]
              ]
          ]
      ]

groceriesView :: GroceriesModel -> Html Message
groceriesView model =
  HE.fragment
    [ HE.main [ HA.class' "flex column container" ]
        [ HE.h1_ [ HE.text "Groceries" ]
        ]
    , HE.footer [ HA.class' "container" ]
        [ HE.nav [ HA.class' "flex spaced justify-center" ]
            [ HE.a
                [ HA.href "/" ]
                [ HE.text "Next days" ]
            , HE.a
                [ HA.href "/groceries" ]
                [ HE.text "Groceries" ]
            ]
        ]
    ]

view :: Model -> Html Message
view model = case model.route of
  HomeM homeM -> homeView model.targetDate homeM
  GroceriesM groceriesM -> groceriesView groceriesM

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

