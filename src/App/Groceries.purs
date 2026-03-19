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
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types as FT

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
  ]

data Message
  = UpdateAmount GroceryId Number
  | CheckedToggled GroceryId

update :: F.Update Model Message
update model =
  case _ of
    UpdateAmount id delta ->
      F.noMessages $ updateGroceryAmount id delta model

    CheckedToggled id ->
      F.noMessages $ toggleGroceryChecked id model

checkbox :: forall m. Array (FT.NodeData m) -> Array (FT.Html m) -> FT.Html m
checkbox = HE.createElement "sl-checkbox"

onChange :: forall m. m -> FT.NodeData m
onChange = HA.createEvent "sl-change"

groceryView :: Grocery -> F.Html Message
groceryView grocery =
  HE.li [ HA.class' "no-list-style" ]
    [ HE.article [ HA.class' "flex spaced items-center justify-space-between" ]
        [ checkbox
            [ HA.class' "grocery-description"
            , HA.class' { checked: grocery.checked }
            , HA.checked grocery.checked
            , HA.id $ printGroceryId grocery.id
            , onChange $ CheckedToggled grocery.id
            ]
            [ HE.text $ fold
                [ grocery.description, " (", show grocery.amount, ")" ]
            ]
        , HE.div
            [ HA.class' "grocery-controls", HA.createProperty "role" "group" ]
            [ HE.button [] [ HE.text "..." ] ]
        ]
    ]

groceriesView :: NonEmptyArray Grocery -> F.Html Message
groceriesView groceries =
  HE.fragment
    [ HE.ul [ HA.class' "no-padding" ] $ map groceryView unchecked
    , HE.h2_ [ HE.text "Done" ]
    , HE.ul [ HA.class' "no-padding" ] $ map groceryView checked
    -- [ HE.ul [ HA.class' "no-padding" ] $ NEA.toArray $ map groceryView groceries
    ]
  where
  { no: unchecked, yes: checked } = NEA.partition _.checked groceries

view :: Model -> F.Html Message
view model =
  HE.fragment
    [ HE.main [ HA.class' "flex column container spaced" ]
        [ HE.h1_ [ HE.text "Groceries" ]
        , HE.a [ HA.href $ Route.print $ GroceriesGenerate ]
            [ HE.text "Generate" ]
        , case NEA.fromArray model of
            Nothing -> HE.text "No groceries have been added yet"
            Just nea -> groceriesView nea
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
