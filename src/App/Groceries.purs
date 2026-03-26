module App.Groceries
  ( Model
  , DragModel
  , Grocery
  , Amount
  , Message
  , GroceryId
  , init
  , update
  , view
  ) where

import Prelude

import App.Layout as Layout
import Data.Route (Route(..))
import Data.Route as Route
import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.MediaType (MediaType(..))
import Data.MediaType.Common as MediaType
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import FFI.DataTransfer as FfiDataTransfer
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Web.Event.Event (Event)
import Web.Event.Event as WE
import Web.HTML.Event.DataTransfer as DataTransfer
import Web.HTML.Event.DragEvent as DragEvent

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

instance Show GroceryId where
  show (MkGroceryId id) = show id

printGroceryId :: GroceryId -> String
printGroceryId (MkGroceryId id) = show id

type Grocery =
  { id :: GroceryId
  , description :: String
  , amount :: Amount
  , checked :: Boolean
  }

newtype DragModel a =
  MkDragModel
    { dragItem :: a
    , dragOverItem :: a
    }

instance Show a => Show (DragModel a) where
  show (MkDragModel { dragItem, dragOverItem }) =
    fold [ "dragItem: ", show dragItem, ", dragOverItem: ", show dragOverItem ]

type Model =
  { groceries :: Array Grocery
  , dragModel :: Maybe (DragModel GroceryId)
  }

updateGroceryAmount :: GroceryId -> Number -> Model -> Model
updateGroceryAmount id delta model =
  model { groceries = updateGrocery <$> model.groceries }
  where
  updateGrocery grocery
    | grocery.id == id = grocery
        { amount = updateAmount delta grocery.amount }
    | otherwise = grocery

toggleGroceryChecked :: GroceryId -> Model -> Model
toggleGroceryChecked id model =
  model { groceries = toggleChecked <$> model.groceries }
  where
  toggleChecked grocery
    | grocery.id == id = grocery
        { checked = not grocery.checked }
    | otherwise = grocery

init :: Model
init =
  { groceries:
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
        , description: "Mushrooms"
        , amount: MkAmount { value: 250.0, unit: Just "g" }
        , checked: false
        }
      , { id: MkGroceryId 4
        , description: "Bell peppers"
        , amount: MkAmount { value: 2.0, unit: Nothing }
        , checked: false
        }
      ]
  , dragModel: Nothing
  }

data Message
  = UpdateAmount GroceryId Number
  | CheckboxClicked GroceryId Event
  | DragEnded Event
  | DragStarted GroceryId Event
  | DraggedOver GroceryId Event
  | DragLeaveOccurred Event

dragType :: String
dragType = "grocery"

setDataTransferData
  :: forall m. MonadEffect m => GroceryId -> Event -> m Unit
setDataTransferData id event = liftEffect do
  for_ dataTransfer \dt -> do
    DataTransfer.setData (MediaType dragType) mempty dt
    DataTransfer.setData MediaType.textPlain printedId dt
    FfiDataTransfer.setEffectAllowed FfiDataTransfer.Move dt
  where
  dataTransfer =
    DragEvent.fromEvent event
      # map DragEvent.dataTransfer
  printedId = printGroceryId id

preventDefaultDropTarget :: forall m. MonadEffect m => Event -> m Unit
preventDefaultDropTarget event =
  when isDropTarget $ liftEffect $ WE.preventDefault event
  where
  isDropTarget =
    DragEvent.fromEvent event
      # map DragEvent.dataTransfer
      # map DataTransfer.types
      <#> Array.elem dragType
      # Maybe.fromMaybe false

update :: F.Update Model Message
update model =
  case _ of
    UpdateAmount id delta ->
      F.noMessages $ updateGroceryAmount id delta model

    CheckboxClicked id event ->
      toggleGroceryChecked id model /\
        [ Nothing <$ (liftEffect $ WE.preventDefault event) ]

    DragStarted id event ->
      updatedModel /\ [ setDataTransferData id event $> Nothing ]
      where
      updatedModel = model
        { dragModel = Just $ MkDragModel { dragItem: id, dragOverItem: id } }

    DragEnded event ->
      model { dragModel = Nothing } /\
        [ preventDefaultDropTarget event $> Nothing ]

    DraggedOver id event ->
      updatedModel /\ [ preventDefaultDropTarget event $> Nothing ]
      where
      setDragOverItem :: DragModel GroceryId -> DragModel GroceryId
      setDragOverItem (MkDragModel dm) = MkDragModel $ dm { dragOverItem = id }
      updatedModel = model
        { dragModel = setDragOverItem <$> model.dragModel }

    DragLeaveOccurred _event ->
      model /\ []

groceryView :: Grocery -> F.Html Message
groceryView grocery =
  HE.li
    [ HA.class' "no-list-style"
    , HA.draggable "true"
    , HA.onDragstart' $ DragStarted grocery.id
    , HA.onDragover' $ DraggedOver grocery.id
    -- , HA.onDragleave' DragLeaveOccurred
    , HA.id $ printGroceryId grocery.id
    ]
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

groceriesView
  :: Maybe (DragModel GroceryId) -> NonEmptyArray Grocery -> F.Html Message
groceriesView _dragModel groceries =
  HE.fragment
    [ HE.ul [ HA.class' "no-padding groceries-list", HA.onDragend' DragEnded ] $
        map groceryView unchecked
    , HE.h2_ [ HE.text "Done" ]
    , HE.ul [ HA.class' "no-padding groceries-list" ] $ map groceryView checked
    , HE.button [ HA.class' "secondary" ] [ HE.text "Clear completed" ]
    ]
  where
  { no: unchecked, yes: checked } = NEA.partition _.checked groceries

view :: Model -> F.Html Message
view model =
  Layout.main $
    HE.div [ HA.class' "flex column" ]
      [ HE.h1_ [ HE.text "Groceries" ]
      , HE.code_
          [ HE.text $ show model.dragModel
          ]
      , HE.div [ HA.class' "flex justify-space-between" ]
          [ HE.a [ HA.href $ Route.print $ GroceriesGenerate ]
              [ HE.text "Generate" ]
          , HE.button [ HA.class' "secondary" ]
              [ HE.text "Edit" ]
          ]
      , case NEA.fromArray model.groceries of
          Nothing -> HE.text "No groceries have been added yet"
          Just nea -> groceriesView model.dragModel nea
      , HE.button [ HA.class' "fab" ] [ HE.text "+" ]
      ]
