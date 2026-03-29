module App.Groceries where

import Prelude

import App.Layout as Layout
import App.Shared as S
import Data.Array (fold, mapWithIndex, (!!))
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
import Web.Event.Event as E
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent as DragEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

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

type DragState a =
  { dragItem :: a
  , dragOverItem :: a
  }

type State =
  { unchecked :: Array Grocery
  , checked :: Array Grocery
  , dragState :: Maybe (DragState Int)
  }

allGroceries :: State -> Array Grocery
allGroceries { checked, unchecked } = checked <> unchecked

startDrag :: Int -> State -> State
startDrag index state =
  state
    { dragState = Just { dragItem: index, dragOverItem: index } }

overDrag :: Int -> State -> State
overDrag index state =
  state { dragState = _ { dragOverItem = index } <$> state.dragState }

endDrag :: State -> State
endDrag state = Maybe.fromMaybe withoutDragState do
  { dragItem, dragOverItem } <- state.dragState
  foundDragItem <- state.unchecked !! dragItem
  updatedGroceries <- Array.deleteAt dragItem state.unchecked
  let
    insertIndex =
      if dragOverItem < dragItem then dragOverItem else dragOverItem

  updatedGroceries' <- Array.insertAt insertIndex foundDragItem updatedGroceries
  pure $ withoutDragState { unchecked = updatedGroceries' }
  where
  withoutDragState = state { dragState = Nothing }

updateGroceryAmount :: GroceryId -> Number -> State -> State
updateGroceryAmount id delta state =
  state { unchecked = updateGrocery <$> state.unchecked }
  where
  updateGrocery grocery
    | grocery.id == id = grocery
        { amount = updateAmount delta grocery.amount }
    | otherwise = grocery

toggleGrocery :: GroceryId -> State -> State
toggleGrocery id state =
  state
    { unchecked = Array.filter (not <<< _.checked) groceries
    , checked = Array.filter _.checked groceries
    }
  where
  toggleChecked grocery
    | grocery.id == id = grocery
        { checked = not grocery.checked }
    | otherwise = grocery
  groceries = toggleChecked <$> allGroceries state

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
  = ToggleGrocery GroceryId MouseEvent
  | StartDrag Int DragEvent
  | OverDrag Int DragEvent
  | EndDrag DragEvent

isPositive :: Int -> Boolean
isPositive x = x > 0

isNegative :: Int -> Boolean
isNegative x = x < 0

dragDelta :: forall a. Ring a => DragState a -> a
dragDelta { dragItem, dragOverItem } = dragItem - dragOverItem

groceryView
  :: forall m
   . MonadAff m
  => Maybe (DragState Int)
  -> Int
  -> Grocery
  -> H.ComponentHTML Action () m
groceryView dragState index grocery =
  let
    isDraggedOver =
      (_.dragOverItem <$> dragState) == Just index
        && (_.dragItem <$> dragState) /= Just index
        && not grocery.checked
    isDragAbove = Maybe.maybe false (dragDelta >>> isPositive) dragState
    isDragBelow = Maybe.maybe false (dragDelta >>> isNegative) dragState
  in
    HH.li
      ( join
          [ [ HP.class_ $ H.ClassName "no-list-style"
            , HP.id $ printGroceryId grocery.id
            , HE.onClick $ ToggleGrocery grocery.id
            ]
          , case grocery.checked of
              false ->
                [ HP.draggable true
                , HE.onDragStart $ StartDrag index
                , HE.onDragOver $ OverDrag index
                ]
              true -> []
          ]
      )
      [ HH.article
          [ S.classes'
              { "flex spaced items-center transparent-border": true
              , "add-above-border": isDraggedOver && isDragAbove
              , "add-below-border": isDraggedOver && isDragBelow
              }
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
  => Maybe (DragState Int)
  -> NonEmptyArray Grocery
  -> H.ComponentHTML Action () m
groceriesView dragState groceries =
  HH.div_
    [ HH.ul
        [ HP.class_ $ H.ClassName "no-padding groceries-list"
        , HE.onDragEnd $ EndDrag
        ] $
        mapWithIndex (groceryView dragState) unchecked
    , HH.h2_ [ HH.text "Done" ]
    , HH.ul [ HP.class_ $ H.ClassName "no-padding groceries-list" ] $
        mapWithIndex (groceryView dragState) checked
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
    { unchecked: Array.filter (not <<< _.checked) dummyGroceries
    , checked: Array.filter _.checked dummyGroceries
    , dragState: Nothing
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    StartDrag index _dragEvent ->
      H.modify_ $ startDrag index
    OverDrag index dragEvent -> do
      H.liftEffect $ E.preventDefault $ DragEvent.toEvent dragEvent
      H.modify_ $ overDrag index
    EndDrag _dragEvent -> do
      H.modify_ $ endDrag
    ToggleGrocery index mouseEvent -> do
      H.liftEffect $ E.preventDefault $ MouseEvent.toEvent mouseEvent
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
        , case NEA.fromArray $ allGroceries state of
            Nothing -> HH.text "No groceries have been added yet"
            Just nea -> groceriesView state.dragState nea
        , HH.button [ HP.class_ $ H.ClassName "fab" ] [ HH.text "+" ]
        ]
