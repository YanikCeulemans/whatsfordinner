module App.GenerateGroceries where

import Prelude

import App.Data as Data
import App.FormField (FormField)
import App.FormField as FormField
import App.Layout as Layout
import App.Shared (eventTargetInputValue, preventDefault)
import App.Shared as S
import Capabilities.Resource.ManageGroceryList (class ManageGroceryList, upsertGroceryList)
import Data.Date (Date)
import Data.Date as Date
import Data.Enum (toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Route as Route
import Data.String as String
import Domain.GroceryList (GroceryList)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)

type State =
  { groceryList :: GroceryList
  , form ::
      { from :: FormField
      , to :: FormField
      }
  }

data Action
  = Initialize
  | SetFromFormFieldState Event
  | SetToFormFieldState Event
  | SubmitForm Event

parseDate :: String -> Maybe Date
parseDate candidate = -- "2027-05-02"

  case String.split (String.Pattern "-") candidate of
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
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      groceryList <- upsertGroceryList Data.dummyListId
      H.modify_ _ { groceryList = groceryList }
    SubmitForm event -> do
      preventDefault event
      -- TODO: implement 
      pure unit
    SetFromFormFieldState event -> do
      -- TODO: implement
      Console.logShow =<< eventTargetInputValue event
      pure unit
    SetToFormFieldState _e ->
      -- TODO: implement
      pure unit

  render :: State -> H.ComponentHTML Action () m
  render { form } =
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
                          , HP.value $ FormField.fieldValue form.from
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
                          , HP.value $ FormField.fieldValue form.to
                          , HP.type_ HP.InputDate
                          ]
                        , FormField.ariaValidity form.to
                        ]
                    )
                ]
            ]
        ]
