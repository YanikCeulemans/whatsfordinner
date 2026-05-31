module App.AddGrocery where

import Prelude

import App.Data as Data
import App.FormField (FormField)
import App.FormField as FormField
import App.Layout as Layout
import App.Shared (preventDefault)
import App.Shared as S
import Capabilities.Navigation (class Navigation, navigate)
import Capabilities.Resource.ManageGroceryList
  ( class ManageGroceryList
  , upsertGrocery
  , upsertGroceryList
  )
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Route as Route
import Data.String as String
import Data.String.NonEmpty as NES
import Data.Traversable (for_)
import Data.Tuple as Tuple
import Domain.Amount as Amount
import Domain.GroceryList (GroceryList, GroceryEntry)
import Domain.GroceryList as GroceryList
import Domain.Id as Id
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

data RemoteData e a
  = NotRequested
  | Loading
  | Error e
  | Success a

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
  -- TODO: reduce to waiting for response
  , remoteData :: RemoteData' Unit
  , groceryList :: Maybe GroceryList
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
buildGrocery state = upsertedGroceryEntry
  where
  upsertedGroceryEntry = Tuple.fst <$> upsertResult
  upsertResult =
    GroceryList.upsertGrocery
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
  | SubmitForm Event

component
  :: forall query input output m
   . MonadAff m
  => ManageGroceryList m
  => Navigation m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize }
    }

  where
  initialState :: input -> State
  initialState _ =
    { id: Nothing
    , form: pristineFormState
    , remoteData: NotRequested
    , groceryList: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      id <- H.liftEffect $ ULID.genULID ULIDW.prng
      groceryList <- upsertGroceryList Data.dummyListId
      H.modify_ _ { id = Just id, groceryList = Just groceryList }

    SetDescriptionFormFieldState event -> do
      value <- S.eventTargetInputValueOrEmpty event
      H.modify_ $ updateForm _ { description = parseNonEmptyString value }

    SetAmountValueFormFieldState event -> do
      value <- S.eventTargetInputValueOrEmpty event
      H.modify_ $ updateForm _ { amountValue = parseAmountValue value }

    SetAmountUnitFormFieldState event -> do
      value <- S.eventTargetInputValueOrEmpty event
      H.modify_ $ updateForm _ { amountUnit = parseAmountUnit value }

    SubmitForm event -> do
      preventDefault event
      groceryCandidate <- buildGrocery <$> H.modify validateForm
      H.modify_ _ { remoteData = Loading }
      for_ groceryCandidate upsertGroceryForDummyList
      H.modify_ _ { remoteData = Success unit }
      navigate $ Route.Groceries
      where
      upsertGroceryForDummyList = upsertGrocery Data.dummyListId

  render :: State -> H.ComponentHTML Action () m
  render { form, remoteData } =
    Layout.main $
      HH.div [ HP.class_ $ H.ClassName "flex column" ]
        [ HH.div [ HP.class_ $ H.ClassName "flex justify-space-between" ]
            [ HH.h1_ [ HH.text "Add grocery" ]
            , S.link Route.Groceries [ HH.text "Cancel" ]
            ]
        , HH.form [ HE.onSubmit SubmitForm ]
            [ HH.label_
                [ HH.text "Description"
                , HH.div [ HP.class_ $ H.ClassName "dropdown" ] -- open
                    [ HH.input []
                    , HH.ul_
                        [ HH.li_ [ HH.text "Tomatoes" ]
                        , HH.li_ [ HH.text "Carrots" ]
                        , HH.li_ [ HH.text "Onions" ]
                        ]
                    ]
                ]
            , HH.label_
                [ HH.text "Description"
                , HH.input
                    ( join $
                        [ [ HE.onInput SetDescriptionFormFieldState
                          , HP.value $ FormField.fieldValue form.description
                          , HP.autofocus true
                          ]
                        , FormField.ariaValid form.description
                        , FormField.ariaInvalid form.description
                        ]
                    )
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
