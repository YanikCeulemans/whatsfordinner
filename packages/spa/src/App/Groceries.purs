module Spa.App.Groceries where

import Prelude

import Common.Amount (Amount(..))
import Common.GroceryEntryId (GroceryEntryId)
import Common.GroceryList (GroceryEntry, GroceryList)
import Common.GroceryList as GroceryList
import Common.GroceryListId (GroceryListId)
import Common.Id as Id
import Common.MealScheduleId (MealScheduleId)
import Common.Space (Space)
import Common.SpaceId (SpaceId)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (class MonadState)
import Data.Array (fold, mapWithIndex)
import Data.Array as Array
import Data.Function (on)
import Data.Lens (Lens')
import Data.Lens as Lens
import Data.Lens.AffineTraversal (AffineTraversal')
import Data.Lens.Record as LensRecord
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Time.Duration (class Duration, Seconds(..), convertDuration)
import Data.Traversable (for_, sequence_)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Spa.App.Layout as Layout
import Spa.App.RemoteRefreshData (RemoteRefreshData(..))
import Spa.App.RemoteRefreshData as RemoteRefreshData
import Spa.App.Shared (preventDefault)
import Spa.App.Shared as S
import Spa.Capabilities.Resource.ManageGroceryList
  ( class ManageGroceryList
  , deleteGroceries
  , updateGroceries
  , upsertGrocery
  , upsertGroceryList
  )
import Spa.Capabilities.Resource.ManageSpaces (class ManageSpaces, loadSpace)
import Spa.Data.Route (GroceriesRoute(..), Route(..), SpaceRoute(..))
import Type.Prelude (Proxy(..))
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

type DragOverState a = { source :: a, target :: a }

data DragState a
  = NotDragging
  | DraggingOverNonTarget a
  | DraggingOverTarget (DragOverState a)

dragOverTarget :: forall a. a -> DragState a -> DragState a
dragOverTarget target = case _ of
  NotDragging -> NotDragging
  DraggingOverNonTarget source -> go source
  DraggingOverTarget { source } -> go source
  where
  go source = DraggingOverTarget { source, target }

type DragEntry =
  { index :: Int
  , item :: GroceryEntry
  }

type GroceryListState =
  { groceryList :: GroceryList
  , groceryListId :: GroceryListId
  , dragState :: DragState DragEntry
  , mealScheduleId :: MealScheduleId
  }

type Input =
  { spaceId :: SpaceId
  , groceryListId :: GroceryListId
  }

data StateSpace
  = StateSpaceId SpaceId
  | StateSpace Space

stateSpaceId :: StateSpace -> SpaceId
stateSpaceId = case _ of
  StateSpaceId id -> id
  StateSpace { id } -> id

type State =
  { groceryListState :: RemoteRefreshData String GroceryListState
  , stateSpace :: StateSpace
  , groceryListId :: GroceryListId
  }

_groceryListState :: Lens' State (RemoteRefreshData String GroceryListState)
_groceryListState = LensRecord.prop (Proxy :: Proxy "groceryListState")

_groceryListStateS :: AffineTraversal' State GroceryListState
_groceryListStateS = _groceryListState <<< RemoteRefreshData._Success

_dragState :: Lens' GroceryListState (DragState DragEntry)
_dragState = LensRecord.prop (Proxy :: Proxy "dragState")

_dragStateS :: AffineTraversal' State (DragState DragEntry)
_dragStateS = _groceryListStateS <<< _dragState

_groceryList :: Lens' GroceryListState GroceryList
_groceryList = LensRecord.prop (Proxy :: Proxy "groceryList")

_groceryListS :: AffineTraversal' State GroceryList
_groceryListS = _groceryListStateS <<< _groceryList

_groceryListId :: Lens' GroceryListState GroceryListId
_groceryListId = LensRecord.prop (Proxy :: Proxy "groceryListId")

_groceryListIdS :: AffineTraversal' State GroceryListId
_groceryListIdS = _groceryListStateS <<< _groceryListId

{--
direction == nothing: untouched
direction == below:
 - anything smaller than source || higher than target -> untouched
 - souce exlusive to target inclusive -> index - 1
 - source's index gets the value of the target's index
direction == above:
 - anything smaller than target || higher than source -> untouched
 - source exclusive to target inclusive -> index + 1
 - source's index gets the value of the target's index
--}
shiftEntry
  :: DragOverState DragEntry -> GroceryEntry -> Tuple Boolean GroceryEntry
shiftEntry dos@{ source, target } entry =
  case direction of
    Nothing -> false /\ entry
    Just Below
      | entry == source.item -> true /\ GroceryList.setEntrySortIndex
          targetIndex
          entry
      | entryIndex > sourceIndex && entryIndex <= targetIndex ->
          true /\ GroceryList.setEntrySortIndex (entryIndex - 1) entry
      | otherwise -> false /\ entry
    Just Above
      | entry == source.item -> true /\ GroceryList.setEntrySortIndex
          targetIndex
          entry
      | entryIndex < sourceIndex && entryIndex >= targetIndex ->
          true /\ GroceryList.setEntrySortIndex (entryIndex + 1) entry
      | otherwise -> false /\ entry
  where
  direction = draggingOverDirection dos
  entryIndex = GroceryList.entrySortIndex entry
  sourceIndex = GroceryList.entrySortIndex source.item
  targetIndex = GroceryList.entrySortIndex target.item

endDrag
  :: GroceryListState
  -> Tuple (Map GroceryEntryId GroceryEntry) GroceryListState
endDrag state =
  case state.dragState of
    NotDragging -> Map.empty /\ state
    DraggingOverNonTarget _ -> Map.empty /\ state { dragState = NotDragging }
    DraggingOverTarget dos ->
      modifiedGroceries /\ state
        { dragState = NotDragging
        , groceryList = shifted
        }
      where
      modifiedGroceries /\ shifted = GroceryList.updateGroceries'
        (shiftEntry dos)
        state.groceryList

getCheckedGroceries :: State -> Maybe (Array GroceryEntry)
getCheckedGroceries s =
  Lens.preview _groceryListS s
    # map GroceryList.partitionGroceriesOnChecked
    # map _.checked

clearCompleted :: GroceryListState -> GroceryListState
clearCompleted state = state
  { groceryList = GroceryList.clearCompleted state.groceryList }

data Action
  = Initialize
  | ToggleGrocery GroceryEntry MouseEvent
  | StartDrag DragEntry DragEvent
  | OverDrag DragEntry DragEvent
  | EndDrag DragEvent
  | ClearCompleted
  | UncheckCompleted
  | HandleMouseDownOnGroceryEntry DragEntry
  | Tick

printAmount :: Amount -> String
printAmount = case _ of
  WithUnit x -> show x.value <> x.unit
  Unitless x -> show x
  ToTaste -> "to taste"

data DragDirection = Above | Below

derive instance Eq DragDirection

draggingOverDirection :: DragOverState DragEntry -> Maybe DragDirection
draggingOverDirection { target, source } = help $ source.index - target.index
  where
  help n
    | n == 0 = Nothing
    | n > 0 = Just Above
    | otherwise = Just Below

dragDirection :: DragEntry -> DragState DragEntry -> Maybe DragDirection
dragDirection dragEntry = case _ of
  NotDragging -> Nothing
  DraggingOverNonTarget _ -> Nothing
  DraggingOverTarget dos@{ target }
    | dragEntry /= target -> Nothing
    | otherwise -> draggingOverDirection dos

groceryView
  :: forall m
   . MonadAff m
  => GroceryListState
  -> Tuple Int GroceryEntry
  -> H.ComponentHTML Action () m
groceryView { dragState } (Tuple index grocery) =
  let
    dragEntry = { index, item: grocery }
    allowDragging = case dragState of
      NotDragging -> false
      _ -> true
    direction
      | GroceryList.entryChecked grocery = Nothing
      | otherwise = dragDirection dragEntry dragState
  in
    HH.li
      ( join
          [ [ HP.class_ $ H.ClassName "no-list-style"
            , HE.onClick $ ToggleGrocery grocery
            ]
          , case GroceryList.entryChecked grocery of
              false ->
                [ HP.draggable $ allowDragging
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
                      [ Just "select-description flex-1 user-select-none"
                      , if GroceryList.entryChecked grocery then Just "checked"
                        else Nothing
                      ]
                  )
              , HP.for $ Id.print $ GroceryList.entryId grocery
              ]
              [ HH.span
                  [ HP.style "margin-right: 10px; cursor: grab;"
                  , HE.onMouseDown
                      $ const
                      $ HandleMouseDownOnGroceryEntry dragEntry
                  , HE.onTouchStart
                      $ const
                      $ HandleMouseDownOnGroceryEntry dragEntry
                  ]
                  [ HH.text "\x283F" ]
              , HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.id $ Id.print $ GroceryList.entryId grocery
                  , HP.checked $ GroceryList.entryChecked grocery
                  ]
              , HH.text $ fold
                  [ GroceryList.entryDescription grocery
                  , " ("
                  , printAmount $ GroceryList.entryAmount grocery
                  , ")"
                  ]
              ]
          ]
      ]

groceriesView
  :: forall m
   . MonadAff m
  => GroceryListState
  -> H.ComponentHTML Action () m
groceriesView state =
  HH.div_
    [ HH.ul
        [ HP.class_ $ H.ClassName "no-padding select-list"
        , HE.onDragEnd $ EndDrag
        ] $
        map (groceryView state) uncheckedGroceries
    , HH.h2_ [ HH.text "Done" ]
    , HH.ul [ HP.class_ $ H.ClassName "no-padding select-list" ] $
        map (groceryView state) checkedGroceries
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
    state.groceryList
      # GroceryList.groceries
      # Array.sortBy (compare `on` GroceryList.entrySortIndex)
      # mapWithIndex Tuple
      # Array.partition (Tuple.snd >>> GroceryList.entryChecked)

layout
  :: forall p i. Boolean -> Array (HH.HTML p i) -> HH.HTML p i -> HH.HTML p i
layout isRefreshing links content =
  HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
    [ HH.div
        [ HP.class_ $ H.ClassName "flex justify-space-between items-center"
        ]
        [ HH.div
            [ HP.class_ $ H.ClassName "flex row spaced items-end" ]
            [ HH.h1 [ HP.class_ $ H.ClassName "no-margin" ]
                [ HH.text "Groceries"
                ]
            , if isRefreshing then HH.text "Refreshing..."
              else HH.text " "
            ]
        , HH.div [ HP.class_ $ H.ClassName "flex spaced" ] $ links
        ]
    , content
    ]

loadGroceriesSpace
  :: forall m
   . ManageGroceryList m
  => ManageSpaces m
  => MonadState State m
  => m Unit
loadGroceriesSpace = do
  state <- H.get
  let
    stateSpace = state.stateSpace
    groceryListState = state.groceryListState
    updatedGroceryListState =
      case groceryListState of
        Success x -> Refreshing x
        RefreshFailed _e x -> Refreshing x
        _ -> Loading

  H.modify_ _ { groceryListState = updatedGroceryListState }

  case stateSpace of
    StateSpaceId spaceId -> do
      foundSpace <- loadSpace spaceId

      case foundSpace of
        Nothing ->
          H.modify_ _
            { groceryListState = Error $ "No space was found for id: " <> show
                spaceId
            }

        Just space -> upsertGroceryListState space

    StateSpace space -> upsertGroceryListState space
  where
  upsertGroceryListState space@{ groceryListId, mealScheduleId } = do
    groceryList <- upsertGroceryList groceryListId
    H.modify_ _
      { groceryListState = Success
          { groceryList
          , groceryListId
          , mealScheduleId
          , dragState: NotDragging
          }
      , stateSpace = StateSpace space
      }

buildMainLayoutConfig :: State -> Layout.MainConfig
buildMainLayoutConfig state =
  (Layout.defaultMainConfig { routing = Just routing })
  where
  routing =
    { spaceId: stateSpaceId state.stateSpace
    , groceryListId: Lens.preview _groceryListIdS state
    }

timer
  :: forall d a. Duration d => d -> a -> HS.Emitter a
timer duration action = HS.makeEmitter help
  where
  help emit = do
    fiber <- Aff.launchAff $ forever do
      Aff.delay $ convertDuration duration
      liftEffect $ emit action
    pure
      $ Aff.launchAff_
      $ Aff.killFiber (Aff.error "unsubcribe was called") fiber

component
  :: forall query output m
   . MonadAff m
  => ManageGroceryList m
  => ManageSpaces m
  => H.Component query Input output m
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
  initialState :: Input -> State
  initialState { spaceId, groceryListId } =
    { groceryListState: NotRequested
    , stateSpace: StateSpaceId spaceId
    , groceryListId
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.subscribe $ timer (Seconds 20.0) Tick
      loadGroceriesSpace

    StartDrag dragEntry _dragEvent ->
      Lens.assign _dragStateS $ DraggingOverNonTarget dragEntry

    OverDrag dragEntry dragEvent -> do
      preventDefault dragEvent
      Lens.modifying _dragStateS $ dragOverTarget dragEntry

    EndDrag _dragEvent -> do
      state <- Lens.use _groceryListState
      for_ state \s@{ groceryListId } -> do
        let
          modifiedGroceries /\ newState = endDrag s
        Lens.assign _groceryListStateS newState
        void $ updateGroceries groceryListId $ syncWith modifiedGroceries
      where
      syncWith modifiedGroceries grocery =
        Map.lookup (GroceryList.entryId grocery) modifiedGroceries
          # Maybe.fromMaybe grocery

    ToggleGrocery grocery mouseEvent -> do
      preventDefault mouseEvent
      Lens.modifying _groceryListS $ GroceryList.set toggledGrocery
      groceryListId <- H.gets $ Lens.preview _groceryListIdS
      for_ groceryListId $ upsertGrocery' toggledGrocery
      where
      toggledGrocery = GroceryList.toggleEntryChecked grocery
      upsertGrocery' a b = upsertGrocery b a

    ClearCompleted -> do
      completed <- H.gets getCheckedGroceries
      Lens.modifying _groceryListS GroceryList.clearCompleted
      groceryListId <- H.gets $ Lens.preview _groceryListIdS
      sequence_ (deleteGroceries <$> groceryListId <*> completed)

    UncheckCompleted -> do
      Lens.modifying _groceryListS
        $ GroceryList.updateEntries GroceryList.uncheckEntry
      groceryListId <- H.gets $ Lens.preview _groceryListIdS
      sequence_
        (updateGroceries <$> groceryListId <*> pure GroceryList.uncheckEntry)

    HandleMouseDownOnGroceryEntry dragEntry -> do
      Lens.assign _dragStateS $ DraggingOverNonTarget dragEntry

    Tick ->
      loadGroceriesSpace

  render :: State -> H.ComponentHTML Action () m
  render state =
    Layout.main' (buildMainLayoutConfig state)
      $ layout isRefreshing links
      $ case state.groceryListState of
          NotRequested -> HH.text "Loading"
          Loading -> HH.text "Loading"
          Error e -> HH.text e
          Success { groceryList } | GroceryList.isEmpty groceryList ->
            HH.text "No groceries have been added yet"
          Refreshing { groceryList } | GroceryList.isEmpty groceryList ->
            HH.text "No groceries have been added yet"
          RefreshFailed _e { groceryList }
            | GroceryList.isEmpty groceryList ->
                HH.div []
                  [ HH.text "Refresh failed, stale data is shown"
                  , HH.text "No groceries have been added yet"
                  ]
          Success groceryListState ->
            groceriesView groceryListState
          Refreshing groceryListState ->
            groceriesView groceryListState
          RefreshFailed _e groceryListState ->
            HH.div []
              [ HH.text "Refresh failed, stale data is shown"
              , groceriesView groceryListState
              ]
    where
    isRefreshing =
      Lens.preview (_groceryListState <<< RemoteRefreshData._Refreshing) state
        # Maybe.isJust
    links = case Lens.preview _groceryListStateS state of
      Nothing -> []
      Just { groceryListId, mealScheduleId } ->
        [ S.link
            ( SpaceRoute (stateSpaceId state.stateSpace)
                $ GroceriesRoute groceryListId
                $ GroceriesGenerate mealScheduleId
            )
            [ HH.text "Generate" ]
        , S.link
            ( SpaceRoute (stateSpaceId state.stateSpace) $ GroceriesRoute
                groceryListId
                AddGrocery
            )
            [ HH.text "Add" ]
        ]
