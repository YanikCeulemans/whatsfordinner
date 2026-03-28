module App.Groceries where

import Prelude

import App.Layout as Layout
import Data.Array (fold, mapWithIndex)
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
  -- TODO: remove the newtype and just keep the record, we don't need this specific show instance anyway
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
  , dragState :: Maybe (DragModel Int)
  }

toggleGrocery :: GroceryId -> State -> State
toggleGrocery id state =
  state { groceries = toggleChecked <$> state.groceries }
  where
  toggleChecked grocery
    | grocery.id == id = grocery
        { checked = not grocery.checked }
    | otherwise = grocery

startDrag :: Int -> State -> State
startDrag index state =
  state
    { dragState = Just $ MkDragModel { dragItem: index, dragOverItem: index } }

overDrag :: Int -> State -> State
overDrag index state =
  state { dragState = updateDragState <$> state.dragState }
  where
  updateDragState (MkDragModel dragState) =
    MkDragModel $
      _ { dragOverItem = index } dragState

endDrag :: State -> State
endDrag state =
  state { dragState = Nothing }

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
  , { id: MkGroceryId 5
    , description: "Zucchini"
    , amount: MkAmount { value: 1.0, unit: Nothing }
    , checked: false
    }
  , { id: MkGroceryId 6
    , description: "Potatoes"
    , amount: MkAmount { value: 2.0, unit: Just "kg" }
    , checked: false
    }
  ]

data Action
  = ToggleGrocery GroceryId
  | StartDrag Int DragEvent
  | OverDrag Int DragEvent
  | EndDrag DragEvent

groceryView
  :: forall m
   . MonadAff m
  => Maybe (DragModel Int)
  -> Int
  -> Grocery
  -> H.ComponentHTML Action () m
groceryView dragState index grocery =
  let
    dragOverItem (MkDragModel ds) = ds.dragOverItem
    isDraggedOver = (dragOverItem <$> dragState) == Just index
  in
    HH.li
      [ HP.classes $ H.ClassName <$> Array.catMaybes
          [ Just "no-list-style"
          , if isDraggedOver then Just
              "add-above-border"
            else Nothing
          ]
      , HP.draggable true
      , HE.onDragStart $ StartDrag index
      , HE.onDragOver $ OverDrag index
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
  => Maybe (DragModel Int)
  -> NonEmptyArray Grocery
  -> H.ComponentHTML Action () m
groceriesView dragModel groceries =
  HH.div_
    [ HH.ul
        [ HP.class_ $ H.ClassName "no-padding groceries-list"
        , HE.onDragEnd $ EndDrag
        ] $
        mapWithIndex (groceryView dragModel) unchecked
    , HH.h2_ [ HH.text "Done" ]
    , HH.ul [ HP.class_ $ H.ClassName "no-padding groceries-list" ] $
        mapWithIndex (groceryView dragModel) checked
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
        { handleAction = handleAction
        }
    }

  where
  initialState :: input -> State
  initialState _ =
    { groceries: dummyGroceries
    , dragState: Nothing
    }

  handleAction
    :: forall childSlots output m
     . MonadAff m
    => Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    StartDrag index dragEvent ->
      H.modify_ $ startDrag index
    OverDrag index dragEvent ->
      H.modify_ $ overDrag index
    EndDrag dragEvent ->
      H.modify_ $ endDrag
    ToggleGrocery index ->
      H.modify_ $ toggleGrocery index

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
