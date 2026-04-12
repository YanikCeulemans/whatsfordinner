module App.Groceries where

import Prelude

import App.Data as Data
import App.Layout as Layout
import App.Shared (preventDefault)
import App.Shared as S
import Capabilities.Resource.ManageGroceryList (class ManageGroceryList, deleteGroceries, updateGroceries, upsertGrocery, upsertGroceryList)
import Control.Alt ((<|>))
import Data.Array (elem, fold, mapWithIndex, (!!))
import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Route (Route(..))
import Data.Route as Route
import Data.Traversable (traverse_)
import Data.ULID as DULID
import Domain.Amount (Amount)
import Domain.Amount as Amount
import Domain.Grocery (Grocery)
import Domain.Grocery as Grocery
import Domain.GroceryId (GroceryId(..))
import Domain.GroceryId as GroceryId
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Simple.ULID (ULID)
import Web.Event.Event (Event)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.InputEvent as InputEvent
import Web.UIEvent.MouseEvent (MouseEvent)

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

toggleGrocery :: Grocery -> State -> State
toggleGrocery grocery state =
  state
    { unchecked = Array.filter (not <<< _.checked) groceries
    , checked = Array.filter _.checked groceries
    }
  where
  toggleChecked grocery'
    | grocery == grocery' = grocery'
        { checked = not grocery.checked }
    | otherwise = grocery'
  groceries = toggleChecked <$> allGroceries state

getGrocery :: GroceryId -> State -> Maybe Grocery
getGrocery id state =
  checkedGrocery <|> uncheckedGrocery
  where
  hasId grocery = Grocery.id grocery == id
  checkedGrocery = Array.find hasId state.checked
  uncheckedGrocery = Array.find hasId state.unchecked

clearCompleted :: State -> State
clearCompleted state = state { checked = [] }

uncheckCompleted :: State -> State
uncheckCompleted state = state
  { unchecked = state.unchecked <> unchecked, checked = [] }
  where
  unchecked = _ { checked = false } <$> state.checked

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
  | ToggleGrocery Grocery MouseEvent
  | StartDrag Int DragEvent
  | OverDrag Int DragEvent
  | EndDrag DragEvent
  | ClearCompleted
  | UncheckCompleted

printAmount :: Amount -> String
printAmount amount =
  case Amount.unit amount of
    Nothing -> show $ Int.ceil value
    Just unit' -> show value <> unit'
  where
  value = Amount.value amount

data DragDirection = Above | Below

derive instance Eq DragDirection

dragDelta :: forall a. Ring a => DragState a -> a
dragDelta { dragItem, dragOverItem } = dragItem - dragOverItem

dragDirection :: Int -> DragState Int -> Maybe DragDirection
dragDirection index { dragItem, dragOverItem }
  | index /= dragOverItem = Nothing
  | otherwise = help $ dragItem - dragOverItem
      where
      help n
        | n == 0 = Nothing
        | n > 0 = Just Above
        | otherwise = Just Below

groceryView
  :: forall m
   . MonadAff m
  => Maybe (DragState Int)
  -> Int
  -> Grocery
  -> H.ComponentHTML Action () m
groceryView dragState index grocery =
  let
    direction
      | grocery.checked = Nothing
      | otherwise = dragDirection index =<< dragState
  in
    HH.li
      ( join
          [ [ HP.class_ $ H.ClassName "no-list-style"
            , HP.id $ GroceryId.print grocery.id
            , HE.onClick $ ToggleGrocery grocery
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
              , "add-above-border": direction == Just Above
              , "add-below-border": direction == Just Below
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
          HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
            [ HH.button
                [ HP.class_ $ H.ClassName "secondary"
                , HE.onClick $ const ClearCompleted
                ]
                [ HH.text "Clear done" ]
            , HH.button
                [ HP.class_ $ H.ClassName "secondary"
                , HE.onClick $ const UncheckCompleted
                ]
                [ HH.text "Uncheck done" ]
            ]
    ]

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
    { unchecked: []
    , checked: []
    , dragState: Nothing
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      groceryList <- upsertGroceryList Data.dummyListId
      H.modify_ $ _
        { unchecked = Array.filter (not <<< _.checked) groceryList
        , checked = Array.filter _.checked groceryList
        }

    StartDrag index _dragEvent ->
      H.modify_ $ startDrag index

    OverDrag index dragEvent -> do
      preventDefault dragEvent
      H.modify_ $ overDrag index

    EndDrag _dragEvent -> do
      H.modify_ $ endDrag

    ToggleGrocery grocery mouseEvent -> do
      -- TODO: Or simply: toggle grocery from param, upsert in state, upsert in remote state?
      preventDefault mouseEvent
      toggledGrocery <-
        getGrocery grocery.id <$> (H.modify $ toggleGrocery grocery)
      traverse_ (upsertGrocery Data.dummyListId) toggledGrocery

    ClearCompleted -> do
      completed <- H.gets _.checked
      H.modify_ clearCompleted
      void $ deleteGroceries Data.dummyListId completed

    UncheckCompleted -> do
      completed <- H.gets _.checked
      H.modify_ uncheckCompleted
      void $ updateGroceries Data.dummyListId
        (uncheckWhenElem (Grocery.id <$> completed))
      where
      uncheckWhenElem ids grocery
        | grocery.id `elem` ids = grocery { checked = false }
        | otherwise = grocery

  render :: State -> H.ComponentHTML Action () m
  render state =
    Layout.main $
      HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
        [ HH.div [ HP.class_ $ H.ClassName "flex justify-space-between" ]
            [ HH.h1_ [ HH.text "Groceries" ]
            , HH.div [ HP.class_ $ H.ClassName "flex spaced" ]
                [ S.link GroceriesGenerate [ HH.text "Generate" ]
                , S.link Route.AddGrocery [ HH.text "Add" ]
                ]
            ]
        , HH.button [ HP.class_ $ H.ClassName "secondary" ]
            [ HH.text "Edit" ]
        , case allGroceries state of
            [] -> HH.text "No groceries have been added yet"
            _ -> groceriesView state
        ]
