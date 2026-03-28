module App.Groceries where

import Prelude

import App.Layout as Layout
import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Route (Route(..))
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.Event.DragEvent (DragEvent)

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

type State =
  { groceries :: Array Grocery
  , dragState :: Maybe (DragModel GroceryId)
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

dummyGroceries :: Array Grocery
dummyGroceries =
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

data Action
  = ToggleGrocery GroceryId
  | StartDrag GroceryId DragEvent
  | OverDrag GroceryId DragEvent

{--
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
--}

groceryView :: forall m. MonadAff m => Grocery -> H.ComponentHTML Action () m
groceryView grocery =
  HH.li
    [ HP.class_ $ H.ClassName "no-list-style"
    , HP.draggable true
    , HE.onDragStart $ StartDrag grocery.id
    , HE.onDragOver $ OverDrag grocery.id
    -- , HP.onDragleave' DragLeaveOccurred
    , HP.id $ printGroceryId grocery.id
    ]
    [ HH.article
        [ HP.class_ $ H.ClassName "flex spaced items-center"
        ]
        [ HH.label
            [ HP.classes $ H.ClassName <$>
                ( Array.catMaybes $
                    [ Just "grocery-description flex-1"
                    , if grocery.checked then Just "checked" else Nothing
                    ]
                )
            , HP.for $ printGroceryId grocery.id
            ]
            [ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.id $ printGroceryId grocery.id
                , HP.checked grocery.checked
                -- , HP.onClick' $ CheckboxClicked grocery.id
                ]
            , HH.text $ fold
                [ grocery.description, " (", show grocery.amount, ")" ]
            ]
        ]
    ]

groceriesView
  :: forall m
   . MonadAff m
  => Maybe (DragModel GroceryId)
  -> NonEmptyArray Grocery
  -> H.ComponentHTML Action () m
groceriesView _dragModel groceries =
  HH.div_
    [ HH.ul
        [ HP.class_ $ H.ClassName "no-padding groceries-list"
        -- , HE.onDragEnd $ EndDrag
        ] $
        map groceryView unchecked
    , HH.h2_ [ HH.text "Done" ]
    , HH.ul [ HP.class_ $ H.ClassName "no-padding groceries-list" ] $ map
        groceryView
        checked
    , HH.button [ HP.class_ $ H.ClassName "secondary" ]
        [ HH.text "Clear completed" ]
    ]
  where
  { no: unchecked, yes: checked } = NEA.partition _.checked groceries

component
  :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

  where
  initialState :: input -> State
  initialState _ =
    { groceries: dummyGroceries
    , dragState: Nothing
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    Layout.main $
      HH.div [ HP.class_ $ H.ClassName "flex column" ]
        [ HH.h1_ [ HH.text "Groceries" ]
        , HH.code_
            [ HH.text $ show state.dragState
            ]
        , HH.div [ HP.class_ $ H.ClassName "flex justify-space-between" ]
            [ HH.a [ HP.href $ Route.print $ GroceriesGenerate ]
                [ HH.text "Generate" ]
            , HH.button [ HP.class_ $ H.ClassName "secondary" ]
                [ HH.text "Edit" ]
            ]
        , case NEA.fromArray state.groceries of
            Nothing -> HH.text "No groceries have been added yet"
            Just nea -> groceriesView state.dragState nea
        , HH.button [ HP.class_ $ H.ClassName "fab" ] [ HH.text "+" ]
        ]
