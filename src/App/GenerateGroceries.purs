module App.GenerateGroceries where

import Prelude

import App.Data as Data
import App.FormField (FormField)
import App.FormField as FormField
import App.Layout as Layout
import App.Shared (eventTargetInputValue, preventDefault)
import App.Shared as S
import Capabilities.Resource.ManageGroceryList (class ManageGroceryList, upsertGroceryList)
import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.Date (Date)
import Data.Date as Date
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Function (on)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Route as Route
import Data.String as String
import Data.Time.Duration (Days(..))
import Domain.GroceryList (GroceryList)
import Domain.Ingredient (Ingredient)
import Domain.MealSchedule as MealSchedule
import Domain.PlannedMeal as PlannedMeal
import Domain.Range (Range)
import Domain.Range as Range
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Web.Event.Event (Event)

data Selection
  = Incomplete { from :: Maybe Date, to :: Maybe Date }
  | Complete (Range Date)

instance Show Selection where
  show = case _ of
    Incomplete x -> "Incomplete " <> show x
    Complete x -> "Complete " <> show x

updateSelectionFrom :: Maybe Date -> Selection -> Selection
updateSelectionFrom from selection = case from, selection of
  Just from', Complete x -> Complete $ Range.create from' (Range.end x)
  from', Complete x -> Incomplete { from: from', to: Just $ Range.end x }
  Just from', Incomplete { to: Just to' } -> Complete $ Range.create from' to'
  from', Incomplete x -> Incomplete $ x { from = from' }

updateSelectionTo :: Maybe Date -> Selection -> Selection
updateSelectionTo to selection = case to, selection of
  Just to', Complete x -> Complete $ Range.create (Range.start x) to'
  to', Complete x -> Incomplete { from: Just $ Range.start x, to: to' }
  Just to', Incomplete { from: Just from' } -> Complete $ Range.create from' to'
  to', Incomplete x -> Incomplete $ x { to = to' }

isComplete :: Selection -> Boolean
isComplete = case _ of
  Complete _ -> true
  _ -> false

type State =
  { groceryList :: GroceryList
  , form ::
      { from :: FormField
      , to :: FormField
      }
  , selection :: Selection
  }

data Action
  = Initialize
  | SetFromFormFieldState Event
  | SetToFormFieldState Event
  | SubmitForm Event

parseDate :: String -> Maybe Date
parseDate candidate =
  case String.split (String.Pattern "-") candidate of -- "2027-05-02"
    [ yearText, monthText, dayText ] ->
      Date.canonicalDate
        <$> year
        <*> month
        <*> day
      where
      year = Int.fromString yearText >>= toEnum
      month = Int.fromString monthText >>= toEnum
      day = Int.fromString dayText >>= toEnum
    _ -> Nothing

data SelectionTarget
  = To
  | From

modifyDate
  :: forall action childslots output m
   . MonadAff m
  => SelectionTarget
  -> Event
  -> H.HalogenM State action childslots output m Unit
modifyDate modifyTarget event = do
  date <- parsedDate
  H.modify_ $ modify date
  where
  parsedDate = eventTargetInputValue event <#> bindFlipped parseDate
  updateSelection = case modifyTarget of
    From -> updateSelectionFrom
    To -> updateSelectionTo
  modify date s = s { selection = updateSelection date s.selection }

toInputDateText :: Date -> String
toInputDateText d =
  format formatter dateTime
  where
  formatter =
    List.fromFoldable $
      Array.intersperse (Placeholder "-")
        [ YearFull, MonthTwoDigits, DayOfMonthTwoDigits ]
  dateTime = DateTime d bottom

toInputDateText' :: SelectionTarget -> Selection -> String
toInputDateText' selectionTarget selection =
  date <#> toInputDateText # Maybe.fromMaybe ""
  where
  date =
    case selectionTarget, selection of
      From, Incomplete x -> x.from
      From, Complete x -> Just $ Range.start x
      To, Incomplete x -> x.to
      To, Complete x -> Just $ Range.end x

mergeIngredients :: List Ingredient -> List Ingredient
mergeIngredients ingredients =
  ingredients
    # List.groupAllBy nameMatches
    # map foldIngredients
  where
  nameMatches = compare `on` _.name
  -- TODO: for now, let's just try to append amounts matching the state ingredient's amount
  help ingredient curr = unsafeCrashWith "TODO"
  foldIngredients xs = NEL.foldl help (NEL.head xs) xs

component
  :: forall query input output m
   . MonadAff m
  => ManageGroceryList m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

  where
  initialState :: input -> State
  initialState _ =
    { groceryList: mempty
    , form:
        { from: FormField.Pristine
        , to: FormField.Pristine
        }
    , selection: Incomplete { from: Nothing, to: Nothing }
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      groceryList <- upsertGroceryList Data.dummyListId
      today <- H.liftEffect nowDate
      H.modify_ _ { groceryList = groceryList, selection = selection today }
      where
      crash _ = unsafeCrashWith "invalid amount of hardcoded days"
      nextWeekFrom = Date.adjust (Days 7.0) >>> Maybe.fromMaybe' crash
      selection today = Complete $ Range.create today $ nextWeekFrom today

    SubmitForm event -> do
      preventDefault event
      selection <- H.gets _.selection
      case selection of
        Complete dateRange -> do
          -- TODO: implement
          pure unit
          where
          meals =
            MealSchedule.toList dateRange Data.mealSchedule
              >>= (PlannedMeal.ingredients >>> List.fromFoldable)
              # mergeIngredients

        Incomplete _ ->
          pure unit

    SetFromFormFieldState event -> do
      modifyDate From event

    SetToFormFieldState event -> do
      modifyDate To event

  render :: State -> H.ComponentHTML Action () m
  render { form, selection } =
    Layout.main $
      HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
        [ HH.div [ HP.class_ $ H.ClassName "flex justify-space-between" ]
            [ HH.h1_ [ HH.text "Generate groceries" ]
            , HH.div [ HP.class_ $ H.ClassName "flex spaced" ]
                [ S.link Route.Groceries [ HH.text "Cancel" ]
                ]
            ]
        , HH.form [ HE.onSubmit SubmitForm ]
            [ HH.label_
                [ HH.text "From"
                , HH.input
                    ( join $
                        [ [ HE.onInput SetFromFormFieldState
                          , HP.value $ toInputDateText' From selection
                          , HP.type_ HP.InputDate
                          ]
                        , FormField.ariaValidity form.from
                        ]
                    )
                ]
            , HH.label_
                [ HH.text "To"
                , HH.input
                    ( join $
                        [ [ HE.onInput SetToFormFieldState
                          , HP.value $ toInputDateText' To selection
                          , HP.type_ HP.InputDate
                          ]
                        , FormField.ariaValidity form.to
                        ]
                    )
                ]
            ]
        , HH.input
            [ HP.type_ HP.InputSubmit
            , HP.value "Generate"
            , HP.disabled $ not $ isComplete selection
            ]
        ]
