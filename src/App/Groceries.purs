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
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple.Nested ((/\))
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

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

type Grocery =
  { id :: GroceryId
  , description :: String
  , amount :: Amount
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

init :: Model
init =
  [ { id: MkGroceryId 1
    , description: "Onion"
    , amount: MkAmount { value: 3.0, unit: Nothing }
    }
  , { id: MkGroceryId 2
    , description: "Carrots"
    , amount: MkAmount { value: 1.0, unit: Just "kg" }
    }
  ]

data Message = UpdateAmount GroceryId Number

update :: F.Update Model Message
update model =
  case _ of
    UpdateAmount id delta ->
      F.noMessages $ updateGroceryAmount id delta model

groceryView :: Grocery -> F.Html Message
groceryView grocery =
  HE.li [ HA.class' "no-list-style" ]
    [ HE.article [ HA.class' "flex spaced items-center justify-space-between" ]
        [ HE.label [ HA.class' "grocery-description" ]
            [ HE.input [ HA.type' "checkbox" ]
            , HE.text $ fold
                [ grocery.description, " (", show grocery.amount, ")" ]
            ]
        , HE.div
            [ HA.class' "grocery-controls", HA.createProperty "role" "group" ]
            [ HE.button [ HA.onClick $ UpdateAmount grocery.id (-1.0) ]
                [ HE.text "-" ]
            , HE.button [ HA.onClick $ UpdateAmount grocery.id 1.0 ]
                [ HE.text "+" ]
            ]
        ]
    ]

view :: Model -> F.Html Message
view model =
  HE.fragment
    [ HE.main [ HA.class' "flex column container spaced" ]
        [ HE.h1_ [ HE.text "Groceries" ]
        , HE.a [ HA.href $ Route.print $ GroceriesGenerate ]
            [ HE.text "Generate" ]
        , case model of
            [] -> HE.text "No groceries have been added yet"
            _ -> HE.ul [ HA.class' "no-padding" ] $ map groceryView model
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
