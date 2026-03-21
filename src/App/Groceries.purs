module App.Groceries
  ( Model
  , Grocery
  , Amount
  , Message
  , GroceryId
  , init
  , update
  , view
  ) where

import Prelude

import App.Route (Route(..))
import App.Route as Route
import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types as FT
import Web.Event.Event (Event)
import Web.Event.Event as WE

newtype Amount = MkAmount
  { value :: Number
  , unit :: Maybe String
  }

instance Show Amount where
  show (MkAmount amount) = fold
    [ show amount.value, Maybe.fromMaybe "" amount.unit ]

updateAmount :: Number -> Amount -> Amount
updateAmount delta (MkAmount amount@{ value }) = MkAmount $ amount
  { value = max 1.0 $ value + delta }

newtype GroceryId = MkGroceryId Int

derive instance Eq GroceryId

printGroceryId :: GroceryId -> String
printGroceryId (MkGroceryId id) = show id

type Grocery =
  { id :: GroceryId
  , description :: String
  , amount :: Amount
  , checked :: Boolean
  }

type Model = Array Grocery

updateGroceryAmount :: GroceryId -> Number -> Model -> Model
updateGroceryAmount id delta model =
  updateGrocery <$> model
  where
  updateGrocery grocery
    | grocery.id == id = grocery
        { amount = updateAmount delta grocery.amount }
    | otherwise = grocery

toggleGroceryChecked :: GroceryId -> Model -> Model
toggleGroceryChecked id model =
  toggleChecked <$> model
  where
  toggleChecked grocery
    | grocery.id == id = grocery
        { checked = not grocery.checked }
    | otherwise = grocery

init :: Model
init =
  [ { id: MkGroceryId 1
    , description: "Onion"
    , amount: MkAmount { value: 3.0, unit: Nothing }
    , checked: false
    }
  , { id: MkGroceryId 2
    , description: "Carrots"
    , amount: MkAmount { value: 1.0, unit: Just "kg" }
    , checked: true
    }
  , { id: MkGroceryId 3
    , description: "Carrots"
    , amount: MkAmount { value: 1.0, unit: Just "kg" }
    , checked: true
    }
  , { id: MkGroceryId 4
    , description: "Carrots"
    , amount: MkAmount { value: 1.0, unit: Just "kg" }
    , checked: true
    }
  ]

data Message
  = UpdateAmount GroceryId Number
  | CheckboxClicked GroceryId Event

update :: F.Update Model Message
update model =
  case _ of
    UpdateAmount id delta ->
      F.noMessages $ updateGroceryAmount id delta model

    CheckboxClicked id event ->
      toggleGroceryChecked id model /\
        [ Nothing <$ (liftEffect $ WE.preventDefault event) ]

groceryView :: Grocery -> F.Html Message
groceryView grocery =
  HE.li [ HA.class' "no-list-style" ]
    [ HE.article
        [ HA.class' "flex spaced items-center"
        ]
        [ HE.label
            [ HA.class' "grocery-description flex-1"
            , HA.class' { checked: grocery.checked }
            , HA.for $ printGroceryId grocery.id
            ]
            [ HE.input
                [ HA.type' "checkbox"
                , HA.id $ printGroceryId grocery.id
                , HA.checked grocery.checked
                , HA.onClick' $ CheckboxClicked grocery.id
                ]
            , HE.text $ fold
                [ grocery.description, " (", show grocery.amount, ")" ]
            ]
        ]
    ]

groceriesView :: NonEmptyArray Grocery -> F.Html Message
groceriesView groceries =
  HE.fragment
    [ HE.ul [ HA.class' "no-padding groceries-list" ] $ map groceryView
        unchecked
    , HE.h2_ [ HE.text "Done" ]
    , HE.button [ HA.class' "secondary" ] [ HE.text "Clear completed" ]
    , HE.ul [ HA.class' "no-padding groceries-list" ] $ map groceryView checked
    ]
  where
  { no: unchecked, yes: checked } = NEA.partition _.checked groceries

view :: Model -> F.Html Message
view model =
  HE.fragment
    [ HE.main [ HA.class' "flex column container spaced" ]
        [ HE.h1_ [ HE.text "Groceries" ]
        , HE.div [ HA.class' "flex justify-space-between" ]
            [ HE.a [ HA.href $ Route.print $ GroceriesGenerate ]
                [ HE.text "Generate" ]
            , HE.button [ HA.class' "secondary" ]
                [ HE.text "Edit" ]
            ]
        , case NEA.fromArray model of
            Nothing -> HE.text "No groceries have been added yet"
            Just nea -> groceriesView nea
        , HE.button [ HA.class' "fab" ] [ HE.text "+" ]
        ]
    , HE.footer [ HA.class' "container" ]
        [ HE.nav [ HA.class' "flex spaced justify-center" ]
            [ HE.a
                [ HA.href $ Route.print $ Route.Home ]
                [ HE.text "Next days" ]
            , HE.a
                [ HA.href $ Route.print $ Route.Groceries ]
                [ HE.text "Groceries" ]
            ]
        ]
    ]
