module App.AddGrocery where

import Prelude

import App.Data as Data
import App.FormField (FormField)
import App.FormField as FormField
import App.Layout as Layout
import App.RemoteData (RemoteData(..))
import App.Shared (preventDefault)
import App.Shared as S
import Capabilities.Navigation (class Navigation, navigate)
import Capabilities.Resource.ManageGroceryList
  ( class ManageGroceryList
  , SortedGrocery
  , suggestGroceries
  , upsertGrocery
  , upsertGroceryList
  )
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Route (Route)
import Data.Route as Route
import Data.String as String
import Data.String.NonEmpty as NES
import Data.Traversable (for_)
import Data.Tuple as Tuple
import Domain.Amount as Amount
import Domain.GroceryList (GroceryEntry, GroceryList)
import Domain.GroceryList as GroceryList
import Domain.GroceryListId (GroceryListId)
import Domain.Id as Id
import Domain.SpaceId (SpaceId)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Aria
import Halogen.HTML.Properties.ARIA as HPA
import Simple.ULID (ULID)
import Simple.ULID as ULID
import Simple.ULID.Window as ULIDW
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (MouseEvent)

type RemoteData' a = RemoteData String a

isLoading :: forall e a. RemoteData e a -> Boolean
isLoading = case _ of
  Loading -> true
  _ -> false

type FormState =
  { description :: FormField
  , amountValue :: FormField
  , amountUnit :: FormField
  }

pristineFormState :: FormState
pristineFormState =
  { description: FormField.Pristine
  , amountValue: FormField.Pristine
  , amountUnit: FormField.Pristine
  }

parseNonEmptyString :: String -> FormField
parseNonEmptyString candidate =
  String.trim candidate
    # NES.fromString
    # Maybe.maybe (FormField.Invalid "") (NES.toString >>> FormField.Valid)

parseAmountValue :: String -> FormField
parseAmountValue candidate =
  String.trim candidate
    # NES.fromString
    >>= validateNumber
    # Maybe.maybe (FormField.Invalid candidate)
        (NES.toString >>> FormField.Valid)
  where
  validateNumber v = (NES.toString v # Number.fromString) $> v

parseAmountUnit :: String -> FormField
parseAmountUnit = String.trim >>> FormField.Valid

isFormStateValid :: FormState -> Boolean
isFormStateValid { description, amountValue, amountUnit } =
  [ description, amountValue, amountUnit ]
    # Array.all FormField.isFieldValid

type State =
  { id :: Maybe ULID
  , form :: FormState
  , suggestionSortIndex :: Maybe Int
  -- TODO: reduce to waiting for response
  , remoteData :: RemoteData' Unit
  , grocerySuggestions :: RemoteData' (Array SortedGrocery)
  , groceryList :: Maybe GroceryList
  , suggestionDebounceForkId :: Maybe H.ForkId
  , groceryListId :: GroceryListId
  , routes ::
      { cancel :: Route
      , submit :: Route
      }
  }

type Input =
  { groceryListId :: GroceryListId
  , routes :: { cancel :: Route, submit :: Route }
  }

updateForm :: (FormState -> FormState) -> State -> State
updateForm f state = state { form = f state.form }

validateForm :: State -> State
validateForm state =
  state { form = validatedForm }
  where
  touchField = case _ of
    FormField.Pristine -> FormField.Invalid ""
    other -> other
  validatedForm =
    { description: touchField state.form.description
    , amountValue: touchField state.form.amountValue
    , amountUnit: state.form.amountUnit
    }

buildGrocery :: State -> Maybe GroceryEntry
buildGrocery state = Tuple.fst <$> upsertResult
  where
  upsertResult =
    GroceryList.upsertGrocery' state.suggestionSortIndex
      <$> id
      <*> description
      <*> amount
      <*> state.groceryList
  id = Id.MkId <$> state.id
  description = FormField.validFieldValue state.form.description
  amountUnit =
    state.form.amountUnit
      # FormField.validFieldValue
      # map String.trim
      >>= case _ of
        "" -> Nothing
        other -> Just other
  amountValue =
    state.form.amountValue
      # FormField.validFieldValue
      >>= Number.fromString
  amount =
    case amountUnit of
      Nothing -> Amount.unitless <$> amountValue
      Just unit' -> Amount.withUnit <$> amountValue <*> pure unit'

data Action
  = Initialize
  | SetDescriptionFormFieldState Event
  | SetAmountValueFormFieldState Event
  | SetAmountUnitFormFieldState Event
  | SelectSuggestion SortedGrocery MouseEvent
  | SubmitForm Event

component
  :: forall query output m
   . MonadAff m
  => ManageGroceryList m
  => Navigation m
  => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize }
    }

  where
  initialState :: Input -> State
  initialState { groceryListId, routes } =
    { id: Nothing
    , form: pristineFormState
    , suggestionSortIndex: Nothing
    , remoteData: NotRequested
    , grocerySuggestions: NotRequested
    , groceryList: Nothing
    , suggestionDebounceForkId: Nothing
    , groceryListId
    , routes
    }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      id <- H.liftEffect $ ULID.genULID ULIDW.prng
      groceryList <- upsertGroceryList Data.dummyListId
      H.modify_ _ { id = Just id, groceryList = Just groceryList }

    SetDescriptionFormFieldState event -> do
      value <- S.eventTargetInputValueOrEmpty event
      H.modify_ $ \state ->
        state
          { form = state.form
              { description = parseNonEmptyString value
              }
          , suggestionSortIndex = Nothing
          , grocerySuggestions = Loading
          }

      prevForkId <- H.gets _.suggestionDebounceForkId
      for_ prevForkId H.kill

      if String.null value then
        clearSuggestions
      else
        debouncedSuggest value

      where
      clearSuggestions = H.modify_ _ { grocerySuggestions = NotRequested }
      debouncedSuggest value = do
        forkId <- H.fork do
          H.liftAff $ delay $ Milliseconds 250.0
          suggestions <- suggestGroceries value
          H.modify_ _ { grocerySuggestions = Success suggestions }
        H.modify_ _ { suggestionDebounceForkId = Just forkId }

    SetAmountValueFormFieldState event -> do
      value <- S.eventTargetInputValueOrEmpty event
      H.modify_ $ updateForm _ { amountValue = parseAmountValue value }

    SetAmountUnitFormFieldState event -> do
      value <- S.eventTargetInputValueOrEmpty event
      H.modify_ $ updateForm _ { amountUnit = parseAmountUnit value }

    SelectSuggestion sortedGrocery _ -> do
      H.modify_ $ \state ->
        state
          { form = state.form
              { description = parseNonEmptyString sortedGrocery.description
              }
          , suggestionSortIndex = Just sortedGrocery.sortIndex
          , grocerySuggestions = NotRequested
          }

    SubmitForm event -> do
      preventDefault event
      groceryCandidate <- buildGrocery <$> H.modify validateForm
      H.modify_ _ { remoteData = Loading }
      for_ groceryCandidate upsertGroceryForDummyList
      H.modify_ _ { remoteData = Success unit }
      routes <- H.gets _.routes
      navigate routes.submit
      where
      upsertGroceryForDummyList = upsertGrocery Data.dummyListId

  render :: State -> H.ComponentHTML Action () m
  render { form, remoteData, grocerySuggestions, routes } =
    Layout.main $
      HH.div [ HP.class_ $ H.ClassName "flex column" ]
        [ HH.div [ HP.class_ $ H.ClassName "flex justify-space-between" ]
            [ HH.h1_ [ HH.text "Add grocery" ]
            , S.link routes.cancel [ HH.text "Cancel" ]
            ]
        , HH.form [ HE.onSubmit SubmitForm ]
            [ HH.label_
                [ HH.text "Description"
                , HH.div
                    [ S.classes'
                        { dropdown: true
                        , open: case grocerySuggestions of
                            Success [] -> false
                            Success _ -> true
                            _ -> false
                        }
                    ]
                    [ HH.input
                        [ HE.onInput SetDescriptionFormFieldState
                        , HP.value $ FormField.fieldValue form.description
                        ]
                    , case grocerySuggestions of
                        NotRequested -> HH.text ""
                        Loading -> HH.text ""
                        Error _ -> HH.text ""
                        Success [] -> HH.text ""
                        Success suggestions ->
                          HH.ul
                            [ HP.style "max-height: 50vh;"
                            , S.classes' { "overflow-auto": true }
                            ] $
                            map
                              ( \s -> HH.li [ HE.onClick $ SelectSuggestion s ]
                                  [ HH.text s.description ]
                              )
                              suggestions
                    ]
                ]
            , HH.fieldset [ HPA.role "group" ]
                [ HH.label_
                    [ HH.text "Amount"
                    , HH.input
                        ( join
                            [ [ HP.type_ InputNumber
                              , HE.onInput SetAmountValueFormFieldState
                              , HP.value $ FormField.fieldValue form.amountValue
                              , HP.min 1.0
                              ]
                            , FormField.ariaValid form.amountValue
                            , FormField.ariaInvalid form.amountValue
                            ]
                        )
                    ]
                , HH.label_
                    [ HH.text "Unit"
                    , HH.input
                        ( join
                            [ [ HE.onInput SetAmountUnitFormFieldState
                              , HP.value $ FormField.fieldValue form.amountUnit
                              ]
                            , FormField.ariaValid form.amountUnit
                            , FormField.ariaInvalid form.amountUnit
                            ]
                        )
                    ]
                ]
            , HH.button
                [ HP.type_ ButtonSubmit
                , Aria.busy $ show $ isLoading remoteData
                ]
                [ HH.text "Add" ]
            ]
        ]
