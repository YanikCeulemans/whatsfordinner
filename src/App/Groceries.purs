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
import Data.Array (fold, mapWithIndex)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Data.Route as Route
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Domain.Amount (Amount)
import Domain.Amount as Amount
import Domain.GroceryEntry (GroceryEntry)
import Domain.GroceryEntry as GroceryEntry
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

data DragState a
  = NotDragging
  | DraggingOverNonTarget a
  | DraggingOverTarget { source :: a, target :: a }

dragOverTarget :: forall a. a -> DragState a -> DragState a
dragOverTarget target = case _ of
  NotDragging -> NotDragging
  DraggingOverNonTarget source -> go source
  DraggingOverTarget { source } -> go source
  where
  go source = DraggingOverTarget { source, target }

type DragEntry =
  { index :: Int
  , item :: Grocery
  }

type State =
  { groceryList :: GroceryList
  , dragState :: DragState DragEntry
  }

endDrag :: State -> State
endDrag state =
  case state.dragState of
    NotDragging -> state
    DraggingOverNonTarget _ -> state { dragState = NotDragging }
    DraggingOverTarget { source, target } ->
      state
        { dragState = NotDragging
        , groceryList = shiftedList
        }
      where
      shiftedList =
        GroceryList.insertAt target.index source.item
          $ GroceryList.delete source.item state.groceryList

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
  | StartDrag DragEntry DragEvent
  | OverDrag DragEntry DragEvent
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

dragDirection :: DragEntry -> DragState DragEntry -> Maybe DragDirection
dragDirection dragEntry = case _ of
  NotDragging -> Nothing
  DraggingOverNonTarget _ -> Nothing
  DraggingOverTarget { source, target }
    | dragEntry /= target -> Nothing
    | otherwise ->
        help $ source.index - target.index
        where
        help n
          | n == 0 = Nothing
          | n > 0 = Just Above
          | otherwise = Just Below

groceryView
  :: forall m
   . MonadAff m
  => DragState DragEntry
  -> Tuple Int Grocery
  -> H.ComponentHTML Action () m
groceryView dragState (Tuple index grocery) =
  let
    dragEntry = { index, item: grocery }
    direction
      | Grocery.checked grocery = Nothing
      | otherwise = dragDirection dragEntry dragState
  in
    HH.li
      ( join
          [ [ HP.class_ $ H.ClassName "no-list-style"
            , HE.onClick $ ToggleGrocery grocery
            ]
          , case Grocery.checked grocery of
              false ->
                [ HP.draggable true
                , HE.onDragStart $ StartDrag dragEntry
                , HE.onDragOver $ OverDrag dragEntry
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
                      , if Grocery.checked grocery then Just "checked"
                        else Nothing
                      ]
                  )
              , HP.for $ GroceryId.print $ Grocery.id grocery
              ]
              [ HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.id $ GroceryId.print $ Grocery.id grocery
                  , HP.checked $ Grocery.checked grocery
                  ]
              , HH.text $ fold
                  [ Grocery.description grocery
                  , " ("
                  , printAmount $ Grocery.amount grocery
                  , ")"
                  ]
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
    , dragState: NotDragging
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      groceryList <- upsertGroceryList Data.dummyListId
      H.modify_ _ { groceryList = groceryList }

    StartDrag dragEntry _dragEvent ->
      H.modify_ $ _ { dragState = DraggingOverNonTarget dragEntry }

    OverDrag dragEntry dragEvent -> do
      preventDefault dragEvent
      H.modify_ setDragOver
      where
      setDragOver state = state
        { dragState = dragOverTarget dragEntry state.dragState }

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
      H.modify_ uncheckCompleted
      void $ updateGroceries Data.dummyListId Grocery.uncheck

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
        , case state.groceryList of
            [] -> HH.text "No groceries have been added yet"
            _ -> groceriesView state
        ]
