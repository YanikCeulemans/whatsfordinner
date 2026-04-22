module App.AddGrocery where

import Prelude

import App.Data as Data
import App.Layout as Layout
import App.Shared (preventDefault)
import App.Shared as S
import Capabilities.Navigation (class Navigation, navigate)
import Capabilities.Resource.ManageGroceryList (class ManageGroceryList, upsertGrocery)
import Data.Array as Array
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Route as Route
import Data.String as String
import Data.String.NonEmpty as NES
import Data.Traversable (for_)
import Data.ULID as DULID
import Domain.Amount as Amount
import Domain.GroceryEntry (GroceryEntry)
import Domain.GroceryEntry as GroceryEntry
import Domain.GroceryId (GroceryId(..))
import Domain.GroceryListId (GroceryListId(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Aria
import Halogen.HTML.Properties.ARIA as HPA
import Partial.Unsafe (unsafeCrashWith)
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

data FormField
  = Pristine
  | Invalid String
  | Valid String

fieldValue :: FormField -> String
fieldValue = case _ of
  Pristine -> ""
  Invalid v -> v
  Valid v -> v

isFieldValid :: FormField -> Boolean
isFieldValid = case _ of
  Valid _ -> true
  _ -> false

validFieldValue :: FormField -> Maybe String
validFieldValue = case _ of
  Valid v -> Just v
  _ -> Nothing

ariaValid :: forall r i. FormField -> Array (HP.IProp r i)
ariaValid = case _ of
  Pristine -> []
  Invalid _ -> [ HP.attr (H.AttrName "aria-valid") "false" ]
  Valid _ -> [ HP.attr (H.AttrName "aria-valid") "true" ]

ariaInvalid :: forall r i. FormField -> Array (HP.IProp r i)
ariaInvalid = case _ of
  Pristine -> []
  Invalid _ -> [ HP.attr (H.AttrName "aria-invalid") "true" ]
  Valid _ -> [ HP.attr (H.AttrName "aria-invalid") "false" ]

type FormState =
  { description :: FormField
  , amountValue :: FormField
  , amountUnit :: FormField
  }

pristineFormState :: FormState
pristineFormState =
  { description: Pristine
  , amountValue: Pristine
  , amountUnit: Pristine
  }

parseNonEmptyString :: String -> FormField
parseNonEmptyString candidate =
  String.trim candidate
    # NES.fromString
    # Maybe.maybe (Invalid "") (NES.toString >>> Valid)

parseAmountValue :: String -> FormField
parseAmountValue candidate =
  String.trim candidate
    # NES.fromString
    >>= validateNumber
    # Maybe.maybe (Invalid candidate) (NES.toString >>> Valid)
  where
  validateNumber v = (NES.toString v # Number.fromString) $> v

parseAmountUnit :: String -> FormField
parseAmountUnit = String.trim >>> Valid

isFormStateValid :: FormState -> Boolean
isFormStateValid { description, amountValue, amountUnit } =
  [ description, amountValue, amountUnit ]
    # Array.all isFieldValid

type State =
  { id :: Maybe ULID
  , form :: FormState
  , remoteData :: RemoteData' Unit
  }

updateForm :: (FormState -> FormState) -> State -> State
updateForm f state = state { form = f state.form }

validateForm :: State -> State
validateForm state =
  state { form = validatedForm }
  where
  touchField = case _ of
    Pristine -> Invalid ""
    other -> other
  validatedForm =
    { description: touchField state.form.description
    , amountValue: touchField state.form.amountValue
    , amountUnit: state.form.amountUnit
    }

buildGrocery :: State -> Maybe Grocery
buildGrocery state =
  Grocery.create
    <$> id
    <*> description
    <*> amount
  where
  id = MkGroceryId <$> state.id
  description = validFieldValue state.form.description
  amountUnit =
    state.form.amountUnit
      # validFieldValue
      # map String.trim
      >>= case _ of
        "" -> Nothing
        other -> Just other
  amountValue = state.form.amountValue # validFieldValue >>= Number.fromString
  amount =
    case amountUnit of
      Nothing -> Amount.unitless <$> amountValue
      Just unit' -> Amount.create <$> amountValue <*> pure unit'

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
    }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Initialize -> do
      id <- H.liftEffect $ ULID.genULID ULIDW.prng
      H.modify_ _ { id = Just id }

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
                [ HH.text "description"
                , HH.input
                    ( join $
                        [ [ HE.onInput SetDescriptionFormFieldState
                          , HP.value $ fieldValue form.description
                          , HP.autofocus true
                          ]
                        , ariaValid form.description
                        , ariaInvalid form.description
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
                              , HP.value $ fieldValue form.amountValue
                              , HP.min 1.0
                              ]
                            , ariaValid form.amountValue
                            , ariaInvalid form.amountValue
                            ]
                        )
                    ]
                , HH.label_
                    [ HH.text "Unit"
                    , HH.input
                        ( join
                            [ [ HE.onInput SetAmountUnitFormFieldState
                              , HP.value $ fieldValue form.amountUnit
                              ]
                            , ariaValid form.amountUnit
                            , ariaInvalid form.amountUnit
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
