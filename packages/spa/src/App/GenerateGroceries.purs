module Spa.App.GenerateGroceries where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State as MonadState
import Data.Array as Array
import Data.Date (Date)
import Data.Date as Date
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import Data.Foldable (foldl)
import Data.Foldable as Foldable
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Function (on)
import Data.Int as Int
import Data.Lens as Lens
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Data.Time.Duration (Days(..))
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Now (nowDate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Simple.ULID as ULID
import Simple.ULID.Window as ULIDW
import Spa.App.Layout as Layout
import Spa.App.RemoteData (RemoteData(..))
import Spa.App.RemoteData as RemoteData
import Spa.App.Shared (eventTargetInputValue, preventDefault)
import Spa.App.Shared as S
import Spa.Capabilities.Navigation (class Navigation, navigate)
import Spa.Capabilities.Resource.ManageGroceryList
  ( class ManageGroceryList
  , upsertGroceries
  , upsertGrocery
  , upsertGroceryList
  )
import Spa.Capabilities.Resource.ManageMealSchedule
  ( class ManageMealSchedule
  , loadMealSchedule
  )
import Spa.Data.Route (GroceriesRoute(..), SpaceRoute(..))
import Spa.Data.Route as Route
import Spa.Domain.Amount as Amount
import Spa.Domain.GroceryList (GroceryEntry, GroceryList)
import Spa.Domain.GroceryList as GroceryList
import Spa.Domain.GroceryListId (GroceryListId)
import Spa.Domain.Id as Id
import Spa.Domain.Ingredient (Ingredient)
import Spa.Domain.MealSchedule (MealSchedule)
import Spa.Domain.MealSchedule as MealSchedule
import Spa.Domain.MealScheduleId (MealScheduleId)
import Spa.Domain.PlannedMeal as PlannedMeal
import Spa.Domain.Range (Range)
import Spa.Domain.Range as Range
import Spa.Domain.SpaceId (SpaceId)
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

type Input =
  { spaceId :: SpaceId
  , groceryListId :: GroceryListId
  , mealScheduleId :: MealScheduleId
  }

type State =
  { groceryList :: GroceryList
  , mealSchedule :: RemoteData String MealSchedule
  , selection :: Selection
  , loading :: Boolean
  , spaceId :: SpaceId
  , groceryListId :: GroceryListId
  , mealScheduleId :: MealScheduleId
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
  help ingredient curr =
    case ingredient.amount, curr.amount of
      Amount.WithUnit a, Amount.WithUnit b
        | a.unit == b.unit ->
            ingredient
              { amount = Amount.WithUnit $ a
                  { value = a.value + b.value }
              }
      Amount.Unitless a, Amount.Unitless b -> ingredient
        { amount = Amount.Unitless $ a + b }

      _, _ -> ingredient

  foldIngredients xs =
    case NEL.uncons xs of
      { head, tail } -> foldl help head tail

traverseGroceries
  :: forall m
   . MonadEffect m
  => ManageGroceryList m
  => GroceryListId
  -> GroceryList
  -> List GroceryEntry
  -> List Ingredient
  -> m (List GroceryEntry)
traverseGroceries groceryListId theList acc =
  case _ of
    Nil -> pure acc
    Cons { name, amount } rest -> do
      ulid <- H.liftEffect $ ULID.genULID ULIDW.prng
      let
        Tuple entry newList =
          GroceryList.upsertGrocery (Id.MkId ulid) name amount theList
      upsertGrocery groceryListId entry
      traverseGroceries groceryListId newList (Cons entry acc) rest

upsertIngredient
  :: forall m
   . MonadEffect m
  => Ingredient
  -> StateT GroceryList m GroceryEntry
upsertIngredient { name, amount } = do
  groceryId <- H.liftEffect $ (Id.MkId <$> ULID.genULID ULIDW.prng)
  groceryList <- MonadState.get
  case GroceryList.upsertGrocery groceryId name amount groceryList of
    Tuple groceryEntry updatedList -> do
      MonadState.put updatedList
      pure groceryEntry

upsertIngredients
  :: forall m
   . MonadAff m
  => Array Ingredient
  -> GroceryList
  -> m (Array GroceryEntry)
upsertIngredients ingredients groceryList =
  evalStateT (traverse upsertIngredient ingredients) groceryList

component
  :: forall query output m
   . MonadAff m
  => ManageGroceryList m
  => ManageMealSchedule m
  => Navigation m
  => H.Component query Input output m
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
  initialState :: Input -> State
  initialState { spaceId, groceryListId, mealScheduleId } =
    { groceryList: mempty
    , mealSchedule: NotRequested
    , selection: Incomplete { from: Nothing, to: Nothing }
    , loading: false
    , spaceId
    , groceryListId
    , mealScheduleId
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      { groceryListId, mealScheduleId } <- H.get
      H.modify_ _ { mealSchedule = Loading }
      groceryList <- upsertGroceryList groceryListId
      today <- H.liftEffect nowDate
      H.modify_ _ { groceryList = groceryList, selection = selection today }
      foundMealSchedule <- loadMealSchedule mealScheduleId
      H.modify_ _
        { mealSchedule = RemoteData.note "No such meal schedule"
            foundMealSchedule
        }
      where
      crash _ = unsafeCrashWith "invalid amount of hardcoded days"
      nextWeekFrom = Date.adjust (Days 7.0) >>> Maybe.fromMaybe' crash
      selection today = Complete $ Range.create today $ nextWeekFrom today

    SubmitForm event -> do
      preventDefault event
      { groceryList, selection, mealSchedule, groceryListId } <- H.get
      for_ mealSchedule \mealSchedule' ->
        case selection of
          Complete dateRange -> do
            H.modify_ _ { loading = true }
            groceries <- upsertIngredients ingredients groceryList
            void $ upsertGroceries groceryListId groceries
            H.modify_ _ { loading = false }
            state <- H.get
            navigate $ Route.SpaceRoute state.spaceId $ GroceriesRoute
              state.groceryListId
              Groceries
            where
            ingredients =
              MealSchedule.toList dateRange mealSchedule'
                >>= (PlannedMeal.ingredients >>> List.fromFoldable)
                # mergeIngredients
                # Array.fromFoldable

          Incomplete _ ->
            pure unit

    SetFromFormFieldState event -> do
      modifyDate From event

    SetToFormFieldState event -> do
      modifyDate To event

  render :: State -> H.ComponentHTML Action () m
  render { loading, selection, spaceId, groceryListId, mealSchedule } =
    Layout.main $
      HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
        [ HH.div [ HP.class_ $ H.ClassName "flex justify-space-between" ]
            [ HH.h1_ [ HH.text "Generate groceries" ]
            , HH.div [ HP.class_ $ H.ClassName "flex spaced" ]
                [ S.link
                    ( Route.SpaceRoute spaceId $ GroceriesRoute groceryListId
                        Groceries
                    )
                    [ HH.text "Cancel" ]
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
                        ]
                    )
                ]
            , HH.button
                [ HP.type_ HP.ButtonSubmit
                , HP.disabled $
                    Foldable.or
                      [ not $ isComplete selection
                      , loading
                      , not mealScheduleLoadSucceeded
                      ]
                , HP.attr (H.AttrName "aria-busy") $ show $ Foldable.or
                    [ loading
                    , not mealScheduleLoadSucceeded
                    ]
                ]
                [ case loading of
                    true -> HH.text "Generating..."
                    false -> HH.text "Generate"
                ]
            ]
        ]
    where
    mealScheduleLoadSucceeded = Maybe.isJust $ Lens.preview RemoteData._Success
      mealSchedule
