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
import Control.MonadPlus as CM
import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Foldable (class Foldable, foldMapDefaultR, foldrDefault)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.MediaType (MediaType(..))
import Data.MediaType.Common as MediaType
import Data.Traversable (for_, traverse)
import Data.Tuple.Nested ((/\))
import Debug as Debug
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import FFI.DataTransfer as FfiDataTransfer
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types as FT
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

printGroceryId :: GroceryId -> String
printGroceryId (MkGroceryId id) = show id

type Grocery =
  { id :: GroceryId
  , description :: String
  , amount :: Amount
  }

type Checkable a =
  { item :: a
  , checked :: Boolean
  }

toggleChecked x = x { checked = not x.checked }

data DndState a
  = Item a
  | Placeholder

instance Functor DndState where
  map f = case _ of
    Item x -> Item $ f x
    Placeholder -> Placeholder

instance Foldable DndState where
  foldl f a = case _ of
    Item x -> f a x
    Placeholder -> a
  foldr = foldrDefault
  foldMap = foldMapDefaultR

type Model = Array (Checkable (DndState Grocery))

updateGroceryAmount :: GroceryId -> Number -> Model -> Model
updateGroceryAmount id delta model =
  updateCheckable <$> model
  where
  updateItemAmount item
    | item.id == id = item { amount = updateAmount delta item.amount }
    | otherwise = item
  updateCheckable checkable = checkable
    { item = map updateItemAmount checkable.item }
  updateGrocery grocery
    | grocery.id == id = grocery
        { amount = updateAmount delta grocery.amount }
    | otherwise = grocery

toggleGroceryChecked :: GroceryId -> Model -> Model
toggleGroceryChecked id model =
  toggle <$> model
  where
  toggle checkable =
    if fold (\g -> ?h g == id) false checkable.item then toggleChecked checkable
    else checkable
  toggleChecked grocery
    | grocery.id == id = grocery
        { checked = not grocery.checked }
    | otherwise = grocery

init :: Model
init =
  [ { item: Item
        { id: MkGroceryId 1
        , description: "Onion"
        , amount: MkAmount { value: 3.0, unit: Nothing }
        }

    , checked: false
    }
  , { item: Item
        { id: MkGroceryId 2
        , description: "Carrots"
        , amount: MkAmount { value: 1.0, unit: Just "kg" }
        }
    , checked: true
    }
  , { item: Item
        { id: MkGroceryId 3
        , description: "Carrots"
        , amount: MkAmount { value: 1.0, unit: Just "kg" }
        }
    , checked: false
    }
  , { item: Item
        { id: MkGroceryId 4
        , description: "Carrots"
        , amount: MkAmount { value: 1.0, unit: Just "kg" }
        }
    , checked: false
    }
  ]

data Message
  = UpdateAmount GroceryId Number
  | CheckboxClicked GroceryId Event
  | DragEnded Event
  | DragStarted GroceryId Event

dragType :: String
dragType = "grocery"

setDataTransferData
  :: forall m. MonadEffect m => GroceryId -> Event -> m Unit
setDataTransferData id event = liftEffect do
  Console.log "setting data transfer data"
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
      model /\ [ setDataTransferData id event $> Nothing ]

    DragEnded event ->
      model /\ [ preventDefaultDropTarget event $> Nothing ]

groceryView :: Boolean -> Grocery -> F.Html Message
groceryView checked grocery =
  HE.li
    [ HA.class' "no-list-style"
    , HA.draggable "true"
    , HA.onDragstart' $ DragStarted grocery.id
    ]
    [ HE.article
        [ HA.class' "flex spaced items-center"
        ]
        [ HE.label
            [ HA.class' "grocery-description flex-1"
            , HA.class' { checked: checked }
            , HA.for $ printGroceryId grocery.id
            ]
            [ HE.input
                [ HA.type' "checkbox"
                , HA.id $ printGroceryId grocery.id
                , HA.checked checked
                , HA.onClick' $ CheckboxClicked grocery.id
                ]
            , HE.text $ fold
                [ grocery.description, " (", show grocery.amount, ")" ]
            ]
        ]
    ]

groceriesView :: NonEmptyArray (Checkable (DndState Grocery)) -> F.Html Message
groceriesView groceries =
  HE.fragment
    [ HE.ul [ HA.class' "no-padding groceries-list", HA.onDragend' DragEnded ] $
        map groceryView
          unchecked
    , HE.h2_ [ HE.text "Done" ]
    , HE.ul [ HA.class' "no-padding groceries-list" ] $ map groceryView checked
    , HE.button [ HA.class' "secondary" ] [ HE.text "Clear completed" ]
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
