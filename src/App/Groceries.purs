module App.Groceries where

import Prelude

import App.Layout as Layout
import App.Shared (preventDefault)
import App.Shared as S
import Data.Array (fold, mapWithIndex, (!!))
import Data.Array as Array
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Route (Route(..))
import Data.Route as Route
import Data.ULID as DULID
import Domain.Amount (Amount)
import Domain.Amount as Amount
import Domain.Grocery (Grocery)
import Domain.GroceryId (GroceryId(..))
import Domain.GroceryId as GroceryId
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Simple.ULID (ULID)
import Simple.ULID as ULID
import Simple.ULID.Window as ULIDW
import Web.Event.Event (Event)
import Web.Event.Event as E
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent as DragEvent
import Web.UIEvent.InputEvent as InputEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

eventInputData :: Event -> Maybe String
eventInputData event = InputEvent.fromEvent event >>= InputEvent.data_

type AmountCandidate =
  { value :: String
  , unit :: String
  }

type DragState a =
  { dragItem :: a
  , dragOverItem :: a
  }

type GroceryAddCandidate =
  { description :: String
  , amount :: AmountCandidate
  }

type State =
  { unchecked :: Array Grocery
  , checked :: Array Grocery
  , dragState :: Maybe (DragState Int)
  , groceryAddCandidate :: Maybe GroceryAddCandidate
  , nextId :: Maybe ULID
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

clearCompleted :: State -> State
clearCompleted state = state { checked = [] }

parseULID :: String -> ULID
parseULID = DULID.parse >>> Either.either crash identity
  where
  crash e = unsafeCrashWith $ "invalid hardcoded ULID: " <> e

dummyGroceries :: Array Grocery
dummyGroceries =
  [ { id: MkGroceryId $ parseULID "01KNEQ7KMSBM0Q4XP56C6YP3NG"
    , description: "Onion"
    , amount: Amount.unitless 3.0
    , checked: false
    }
  , { id: MkGroceryId $ parseULID "01KNEQ7KMTPNHDW1X8N4P9G7WV"
    , description: "Carrots"
    , amount: Amount.create 1.0 "kg"
    , checked: true
    }
  , { id: MkGroceryId $ parseULID "01KNEQ7KMTCC0KREZGB172ASD0"
    , description: "Mushrooms"
    , amount: Amount.create 250.0 "g"
    , checked: false
    }
  , { id: MkGroceryId $ parseULID "01KNEQ7KMTP534FYCKA4ZCBEFS"
    , description: "Bell peppers"
    , amount: Amount.unitless 2.0
    , checked: false
    }
  , { id: MkGroceryId $ parseULID "01KNEQ7KMTG18271MG7NEEBV05"
    , description: "Zucchini"
    , amount: Amount.unitless 1.0
    , checked: false
    }
  , { id: MkGroceryId $ parseULID "01KNEQ7KMT0XDE57C3HJPE6GVQ"
    , description: "Potatoes"
    , amount: Amount.create 2.0 "kg"
    , checked: false
    }
  ]

data Action
  = Initialize
  | ToggleGrocery GroceryId MouseEvent
  | StartDrag Int DragEvent
  | OverDrag Int DragEvent
  | EndDrag DragEvent
  | ClearCompleted
  | ShowAddGrocery
  | CancelAddGrocery MouseEvent
  | UpdateGroceryAddCandidateDescription Event
  | UpdateGroceryAddCandidateAmountValue Event
  | UpdateGroceryAddCandidateAmountUnit Event

isPositive :: Int -> Boolean
isPositive x = x > 0

isNegative :: Int -> Boolean
isNegative x = x < 0

dragDelta :: forall a. Ring a => DragState a -> a
dragDelta { dragItem, dragOverItem } = dragItem - dragOverItem

printAmount :: Amount -> String
printAmount amount =
  [ Just $ show $ Amount.value amount, Amount.unit amount ]
    # Array.catMaybes
    # Array.intercalate " "

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
            , HP.id $ GroceryId.print grocery.id
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
              , HP.for $ GroceryId.print grocery.id
              ]
              [ HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.id $ GroceryId.print grocery.id
                  , HP.checked grocery.checked
                  -- , HP.onClick' $ CheckboxClicked grocery.id
                  ]
              , HH.text $ fold
                  [ grocery.description, " (", printAmount grocery.amount, ")" ]
              ]
          ]
      ]

groceriesView
  :: forall m
   . MonadAff m
  => State
  -> H.ComponentHTML Action () m
groceriesView state =
  HH.div_
    [ HH.ul
        [ HP.class_ $ H.ClassName "no-padding groceries-list"
        , HE.onDragEnd $ EndDrag
        ] $
        mapWithIndex (groceryView state.dragState) state.unchecked
    , HH.h2_ [ HH.text "Done" ]
    , HH.ul [ HP.class_ $ H.ClassName "no-padding groceries-list" ] $
        mapWithIndex (groceryView state.dragState) state.checked
    , case state.checked of
        [] -> HH.text ""
        _ ->
          HH.button
            [ HP.class_ $ H.ClassName "secondary"
            , HE.onClick $ const ClearCompleted
            ]
            [ HH.text "Clear completed" ]
    ]

groceryAddView
  :: forall m. MonadAff m => GroceryAddCandidate -> H.ComponentHTML Action () m
groceryAddView groceryAddCandidate =
  HH.div [ HP.class_ $ H.ClassName "flex column" ]
    [ HH.h1_ [ HH.text "Add grocery" ]
    , HH.code_ [ HH.text $ groceryAddCandidate.description ]
    , HH.form []
        [ HH.label_
            [ HH.text "description"
            , HH.input
                [ HE.onInput UpdateGroceryAddCandidateDescription
                , HP.value $ groceryAddCandidate.description
                ]
            ]
        , HH.label_
            [ HH.text "Amount"
            , HH.input
                [ HP.type_ InputNumber
                , HE.onInput UpdateGroceryAddCandidateAmountValue
                , HP.value $ groceryAddCandidate.amount.value
                , HP.min 1.0
                ]
            ]
        , HH.label_
            [ HH.text "Unit"
            , HH.input
                [ HE.onInput UpdateGroceryAddCandidateAmountUnit
                -- , HP.value $ show $ amountUnit groceryAddCandidate.amount
                , HP.value groceryAddCandidate.amount.unit
                ]
            ]
        , HH.input [ HP.type_ InputButton, HP.value "Add" ]
        , HH.button
            [ HP.class_ $ H.ClassName "secondary"
            , HE.onClick $ CancelAddGrocery
            ]
            [ HH.text "Cancel" ]
        ]
    ]

component
  :: forall query input output m. MonadAff m => H.Component query input output m
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
    { unchecked: Array.filter (not <<< _.checked) dummyGroceries
    , checked: Array.filter _.checked dummyGroceries
    , dragState: Nothing
    , groceryAddCandidate: Nothing
    , nextId: Nothing
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      id <- H.liftEffect $ ULID.genULID ULIDW.prng
      H.modify_ _ { nextId = Just id }

    StartDrag index _dragEvent ->
      H.modify_ $ startDrag index

    OverDrag index dragEvent -> do
      preventDefault dragEvent
      H.modify_ $ overDrag index

    EndDrag _dragEvent -> do
      H.modify_ $ endDrag

    ToggleGrocery index mouseEvent -> do
      preventDefault mouseEvent
      H.modify_ $ toggleGrocery index

    ClearCompleted ->
      H.modify_ clearCompleted

    ShowAddGrocery ->
      H.modify_ _
        { groceryAddCandidate = Just
            { description: "", amount: { value: "1", unit: "" } }
        }
    CancelAddGrocery mouseEvent -> do
      preventDefault mouseEvent
      H.modify_ _ { groceryAddCandidate = Nothing }

    UpdateGroceryAddCandidateDescription event -> do
      value <- Maybe.fromMaybe "" <$> S.eventTargetInputValue event

      H.modify_ \s -> s
        { groceryAddCandidate = updateDescription value <$>
            s.groceryAddCandidate
        }

      where
      updateDescription value = _ { description = value }

    UpdateGroceryAddCandidateAmountValue event -> do
      value <- S.eventTargetInputValue event
      case value of
        Nothing -> pure unit
        Just v -> do
          H.modify_ \s -> s
            { groceryAddCandidate = updateAmountValue v <$>
                s.groceryAddCandidate
            }

      where
      updateAmountValue value x = x { amount = x.amount { value = value } }

    UpdateGroceryAddCandidateAmountUnit event -> do
      pure unit

  render :: State -> H.ComponentHTML Action () m
  render state =
    Layout.main $
      case state.groceryAddCandidate of
        Nothing ->
          HH.div [ HP.class_ $ H.ClassName "flex column" ]
            [ HH.div [ HP.class_ $ H.ClassName "flex justify-space-between" ]
                [ HH.h1_ [ HH.text "Groceries" ]
                , S.link Route.AddGrocery [ HH.text "Add" ]
                ]
            , HH.div [ HP.class_ $ H.ClassName "flex justify-space-between" ]
                [ HH.a [ HP.href $ Route.print $ GroceriesGenerate ]
                    [ HH.text "Generate" ]
                , HH.button [ HP.class_ $ H.ClassName "secondary" ]
                    [ HH.text "Edit" ]
                ]
            , case allGroceries state of
                [] -> HH.text "No groceries have been added yet"
                _ -> groceriesView state
            ]
        Just groceryAddCandidate -> groceryAddView groceryAddCandidate
