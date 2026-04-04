module App.AddGrocery where

import Prelude

import App.Layout as Layout
import App.Shared as S
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Route as Route
import Data.String as String
import Data.String.NonEmpty as NES
import Debug as Debug
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Simple.ULID (ULID)
import Web.Event.Event (Event)

data FormField
  = Pristine
  | Invalid String
  | Valid String

fieldValue :: FormField -> String
fieldValue = case _ of
  Pristine -> ""
  Invalid v -> v
  Valid v -> v

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

type Input = ULID

type State =
  { id :: ULID
  , form :: FormState
  }

updateForm :: (FormState -> FormState) -> State -> State
updateForm f state = state { form = f state.form }

data Action
  = SetDescriptionFormFieldState Event
  | SetAmountValueFormFieldState Event
  | SetAmountUnitFormFieldState Event

component
  :: forall query output m. MonadAff m => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

  where
  initialState id =
    { id
    , form:
        { description: Pristine
        , amountValue: Pristine
        , amountUnit: Pristine
        }
    }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    SetDescriptionFormFieldState event -> do
      value <- S.eventTargetInputValueOrEmpty event
      H.modify_ $ updateForm _ { description = parseNonEmptyString value }

    SetAmountValueFormFieldState event -> do
      value <- S.eventTargetInputValueOrEmpty event
      H.modify_ $ updateForm _ { amountValue = parseAmountValue value }

    SetAmountUnitFormFieldState event -> do
      value <- S.eventTargetInputValueOrEmpty event
      H.modify_ $ updateForm _ { amountUnit = parseAmountUnit value }

  render :: State -> H.ComponentHTML Action () m
  render s@{ form } =
    let
      _ = Debug.spy "state" s
    in
      Layout.main $
        HH.div [ HP.class_ $ H.ClassName "flex column" ]
          [ HH.div [ HP.class_ $ H.ClassName "flex justify-space-between" ]
              [ HH.h1_ [ HH.text "Add grocery" ]
              , S.link Route.Groceries [ HH.text "Cancel" ]
              ]
          , HH.form []
              [ HH.label_
                  [ HH.text "description"
                  , HH.input
                      ( join $
                          [ [ HE.onInput SetDescriptionFormFieldState
                            , HP.value $ fieldValue form.description
                            ]
                          , ariaValid form.description
                          , ariaInvalid form.description
                          ]
                      )
                  ]
              , HH.label_
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
              , HH.input [ HP.type_ InputButton, HP.value "Add" ]
              ]
          ]
