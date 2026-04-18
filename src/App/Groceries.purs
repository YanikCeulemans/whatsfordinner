module App.Groceries where

import Prelude

import App.Data as Data
import App.Layout as Layout
import App.Shared (preventDefault)
import App.Shared as S
import Capabilities.Resource.ManageGroceryList
  ( class ManageGroceryList
  , deleteGroceries
  , updateGroceries
  , upsertGrocery
  , upsertGroceryList
  )
import Data.Array (elem, fold, mapWithIndex, (!!))
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Route (Route(..))
import Data.Route as Route
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Domain.Amount (Amount)
import Domain.Amount as Amount
import Domain.Grocery (Grocery)
import Domain.Grocery as Grocery
import Domain.GroceryId as GroceryId
import Domain.GroceryList (GroceryList)
import Domain.GroceryList as GroceryList
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

type DragState a =
  { dragItem :: a
  , dragOverItem :: a
  }

type State =
  { groceryList :: GroceryList
  , dragState :: Maybe (DragState Int)
  }

allGroceries :: State -> GroceryList
allGroceries { groceryList } = groceryList

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
  foundDragItem <- state.groceryList !! dragItem
  let
    updatedList =
      GroceryList.insertAt dragOverItem foundDragItem
        $ GroceryList.delete foundDragItem state.groceryList

  pure $ withoutDragState { groceryList = updatedList }
  where
  withoutDragState = state { dragState = Nothing }

setGrocery :: Grocery -> State -> State
setGrocery grocery s =
  s { groceryList = GroceryList.set grocery s.groceryList }

clearCompleted :: State -> State
clearCompleted state = state
  { groceryList = GroceryList.clearCompleted state.groceryList }

uncheckCompleted :: State -> State
uncheckCompleted state = state
  { groceryList = map Grocery.uncheck state.groceryList }

data Action
  = Initialize
  | ToggleGrocery Grocery MouseEvent
  | StartDrag Int DragEvent
  | OverDrag Int DragEvent
  | EndDrag DragEvent
  -- TODO: implement drag leave to prevent dragging items out of lists
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
  -> Tuple Int Grocery
  -> H.ComponentHTML Action () m
groceryView dragState (Tuple index grocery) =
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
        map (groceryView state.dragState) uncheckedGroceries
    , HH.h2_ [ HH.text "Done" ]
    , HH.ul [ HP.class_ $ H.ClassName "no-padding groceries-list" ] $
        map (groceryView state.dragState) checkedGroceries
    , case checkedGroceries of
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

  where
  { no: uncheckedGroceries, yes: checkedGroceries } =
    mapWithIndex Tuple state.groceryList
      # Array.partition (Tuple.snd >>> Grocery.checked)

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
    { groceryList: mempty
    , dragState: Nothing
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      groceryList <- upsertGroceryList Data.dummyListId
      H.modify_ _ { groceryList = groceryList }

    StartDrag index _dragEvent ->
      H.modify_ $ startDrag index

    OverDrag index dragEvent -> do
      preventDefault dragEvent
      H.modify_ $ overDrag index

    EndDrag _dragEvent -> do
      H.modify_ $ endDrag

    ToggleGrocery grocery mouseEvent -> do
      preventDefault mouseEvent
      H.modify_ $ setGrocery toggledGrocery
      upsertGrocery Data.dummyListId toggledGrocery
      where
      toggledGrocery = Grocery.toggleChecked grocery

    ClearCompleted -> do
      completed <- H.gets getCheckedGroceries
      H.modify_ clearCompleted
      void $ deleteGroceries Data.dummyListId completed
      where
      getCheckedGroceries s =
        GroceryList.partitionGroceriesOnChecked s.groceryList
          # _.checked

    UncheckCompleted -> do
      completed <- H.gets getCheckedGroceries
      H.modify_ uncheckCompleted
      void $ updateGroceries Data.dummyListId
        (uncheckWhenElem (Grocery.id <$> completed))
      where
      getCheckedGroceries s =
        GroceryList.partitionGroceriesOnChecked s.groceryList
          # _.checked
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
