module Spa.App.Groceries where

import Prelude

import Data.Array (fold, mapWithIndex)
import Data.Array as Array
import Data.Function (on)
import Data.Lens (Lens', _Just)
import Data.Lens as Lens
import Data.Lens.AffineTraversal (AffineTraversal')
import Data.Lens.Record as LensRecord
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Time.Duration (Seconds(..), convertDuration)
import Data.Traversable (for_, sequence_)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Spa.App.Layout as Layout
import Spa.App.RemoteData (RemoteData(..))
import Spa.App.RemoteData as RemoteData
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
import Spa.Domain.Amount (Amount(..))
import Spa.Domain.GroceryList (GroceryEntry, GroceryList)
import Spa.Domain.GroceryList as GroceryList
import Spa.Domain.GroceryListId (GroceryListId)
import Spa.Domain.Id as Id
import Spa.Domain.MealScheduleId (MealScheduleId)
import Spa.Domain.SpaceId (SpaceId)
import Spa.FFI.WebSocket (WebSocket)
import Spa.FFI.WebSocket as WS
import Spa.FFI.WebSocket.Types.CloseEvent (CloseEvent)
import Spa.FFI.WebSocket.Types.CloseEvent as WSTC
import Spa.FFI.WebSocket.Types.MessageEvent (MessageEvent)
import Spa.FFI.WebSocket.Types.ReadyState (ReadyState(..))
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

type WebSocketState =
  { webSocket :: WebSocket
  , readyState :: ReadyState
  }

type GroceryListState =
  { groceryList :: GroceryList
  , groceryListId :: GroceryListId
  , dragState :: DragState DragEntry
  , webSocketState :: Maybe WebSocketState
  , mealScheduleId :: MealScheduleId
  }

type Input =
  { spaceId :: SpaceId
  , groceryListId :: GroceryListId
  }

type State =
  { groceryListState :: RemoteData String GroceryListState
  , spaceId :: SpaceId
  , groceryListId :: GroceryListId
  }

_groceryListState :: Lens' State (RemoteData String GroceryListState)
_groceryListState = LensRecord.prop (Proxy :: Proxy "groceryListState")

_groceryListStateS :: AffineTraversal' State GroceryListState
_groceryListStateS = _groceryListState <<< RemoteData._Success

_webSocketState :: Lens' GroceryListState (Maybe WebSocketState)
_webSocketState = LensRecord.prop (Proxy :: Proxy "webSocketState")

_webSocketStateS :: AffineTraversal' State (Maybe WebSocketState)
_webSocketStateS = _groceryListStateS <<< _webSocketState

_webSocket :: Lens' WebSocketState WebSocket
_webSocket = LensRecord.prop (Proxy :: Proxy "webSocket")

_webSocketS :: AffineTraversal' State WebSocket
_webSocketS =
  _groceryListState
    <<< RemoteData._Success
    <<< _webSocketState
    <<< _Just
    <<< _webSocket

_readyState :: Lens' WebSocketState ReadyState
_readyState = LensRecord.prop (Proxy :: Proxy "readyState")

_readyStateS :: AffineTraversal' State ReadyState
_readyStateS =
  _groceryListState
    <<< RemoteData._Success
    <<< _webSocketState
    <<< _Just
    <<<
      _readyState

_dragState :: Lens' GroceryListState (DragState DragEntry)
_dragState = LensRecord.prop (Proxy :: Proxy "dragState")

_dragStateS :: AffineTraversal' State (DragState DragEntry)
_dragStateS =
  _groceryListState <<< RemoteData._Success <<< _dragState

_groceryList :: Lens' GroceryListState GroceryList
_groceryList = LensRecord.prop (Proxy :: Proxy "groceryList")

_groceryListS :: AffineTraversal' State GroceryList
_groceryListS =
  _groceryListState <<< RemoteData._Success <<< _groceryList

_groceryListId :: Lens' GroceryListState GroceryListId
_groceryListId = LensRecord.prop (Proxy :: Proxy "groceryListId")

_groceryListIdS :: AffineTraversal' State GroceryListId
_groceryListIdS =
  _groceryListState <<< RemoteData._Success <<< _groceryListId

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

endDrag :: GroceryListState -> Tuple (Array GroceryEntry) GroceryListState
endDrag state =
  case state.dragState of
    NotDragging -> [] /\ state
    DraggingOverNonTarget _ -> [] /\ state { dragState = NotDragging }
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
  | Finalize
  | ConnectWebSocket GroceryListId
  | ToggleGrocery GroceryEntry MouseEvent
  | StartDrag DragEntry DragEvent
  | OverDrag DragEntry DragEvent
  | EndDrag DragEvent
  | ClearCompleted
  | UncheckCompleted
  | HandleMouseDownOnGroceryEntry DragEntry
  | MessageReceived MessageEvent
  | WebSocketOpened
  | WebSocketClosed GroceryListId CloseEvent

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
      # Array.sortBy (compare `on` GroceryList.entrySortIndex)
      # mapWithIndex Tuple
      # Array.partition (Tuple.snd >>> GroceryList.entryChecked)

isNoLongerInterested :: CloseEvent -> Boolean
isNoLongerInterested closeEvent =
  WSTC.codeMatches WSTC.NoLongerInterested closeEvent

-- TODO: What happens when there is no internet?
connectWebSocket :: forall m. MonadEffect m => GroceryListId -> m WebSocket
connectWebSocket groceryListId = H.liftEffect
  $ WS.mk
  $ fold [ "ws://localhost:5000/ws/", Id.print groceryListId ]

subscribeToWebSocketEvent
  :: forall event state action slots output m
   . WS.EventConfig event
  -> (event -> Maybe action)
  -> WebSocket
  -> H.HalogenM state action slots output m Unit
subscribeToWebSocketEvent eventConfig buildAction ws =
  void $ H.subscribe $ HQE.eventListener wsEventType wsEventTarget handleEvent
  where
  wsEventType = eventConfig.eventType
  wsEventTarget = WS.toEventTarget ws
  handleEvent event = eventConfig.fromEvent event >>= buildAction

updateReadyState
  :: forall childSlots output m
   . MonadEffect m
  => H.HalogenM State Action childSlots output m Unit
updateReadyState = do
  webSocket <- H.gets $ Lens.preview _webSocketS
  for_ webSocket \ws -> do
    readyState <- H.liftEffect $ WS.readyState ws
    Lens.assign _readyStateS readyState

buildMainLayoutConfig :: State -> Layout.MainConfig
buildMainLayoutConfig state =
  (Layout.defaultMainConfig { routing = Just routing })
  where
  routing =
    { spaceId: state.spaceId
    , groceryListId: Lens.preview _groceryListIdS state
    }

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
        , finalize = Just Finalize
        }
    }

  where
  initialState :: Input -> State
  initialState { spaceId, groceryListId } =
    { groceryListState: NotRequested
    , spaceId
    , groceryListId
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      H.modify_ _ { groceryListState = Loading }
      foundSpace <- loadSpace =<< H.gets _.spaceId
      for_ foundSpace \{ groceryListId, mealScheduleId } -> do
        groceryList <- upsertGroceryList groceryListId
        H.modify_ _
          { groceryListState = Success
              { groceryList
              , groceryListId
              , mealScheduleId
              , dragState: NotDragging
              , webSocketState: Nothing
              }
          }
        handleAction $ ConnectWebSocket groceryListId

    Finalize -> do
      groceryListState <- H.gets _.groceryListState
      let
        webSocket =
          groceryListState
            # map _.webSocketState
            # RemoteData.toMaybe
            # join
            # map _.webSocket
      H.liftEffect $ for_ webSocket $ WS.close WSTC.NoLongerInterested

    -- TODO: WebSocket to capability?
    ConnectWebSocket groceryListId -> do
      -- TODO: handle socket error event, try to reconnect?
      ws <- connectWebSocket groceryListId
      subscribeToWebSocketEvent
        WS.eventConfigs.open
        (const WebSocketOpened >>> Just)
        ws
      subscribeToWebSocketEvent
        WS.eventConfigs.message
        (MessageReceived >>> Just)
        ws
      subscribeToWebSocketEvent
        WS.eventConfigs.close
        (WebSocketClosed groceryListId >>> Just)
        ws

      readyState <- H.liftEffect $ WS.readyState ws
      Lens.assignJust _webSocketStateS { webSocket: ws, readyState }

    WebSocketOpened -> do
      Console.log "web socket opened"
      updateReadyState

    WebSocketClosed groceryListId closeEvent -> do
      Console.logShow { msg: "web socket closed", closeEvent }
      updateReadyState
      unless (isNoLongerInterested closeEvent) do
        Console.log "retrying web socket connection"
        H.liftAff $ Aff.delay $ convertDuration $ Seconds 5.0
        handleAction $ ConnectWebSocket groceryListId

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
        Array.find (eq (grocery :: GroceryEntry)) modifiedGroceries
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
      Lens.modifying _groceryListS $ map GroceryList.uncheckEntry
      groceryListId <- H.gets $ Lens.preview _groceryListIdS
      sequence_
        (updateGroceries <$> groceryListId <*> pure GroceryList.uncheckEntry)

    HandleMouseDownOnGroceryEntry dragEntry -> do
      Lens.assign _dragStateS $ DraggingOverNonTarget dragEntry

    MessageReceived event ->
      Console.log $ fold [ "msg received: ", event.data ]

  render :: State -> H.ComponentHTML Action () m
  render state =
    Layout.main' (buildMainLayoutConfig state) $
      HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
        [ HH.div
            [ HP.class_ $ H.ClassName "flex justify-space-between items-center"
            ]
            [ HH.div
                [ HP.class_ $ H.ClassName "flex row spaced items-baseline" ]
                [ HH.h1 [ HP.class_ $ H.ClassName "no-margin" ]
                    [ HH.text "Groceries" ]
                , HH.div
                    [ S.classes'
                        { "live-status": true
                        , "pico-background-lime-100": isLive
                        , "pico-background-pink-450": not isLive
                        }
                    ]
                    []
                ]
            , HH.div [ HP.class_ $ H.ClassName "flex spaced" ] $ groceryLinks
                (Lens.preview _groceryListStateS state)
            ]
        , case state.groceryListState of
            NotRequested -> HH.text "Loading"
            Loading -> HH.text "Loading"
            Error e -> HH.text e
            Success { groceryList: [] } -> HH.text
              "No groceries have been added yet"
            Success groceryListState -> groceriesView groceryListState
        ]
    where
    groceryLinks = case _ of
      Nothing -> []
      Just { groceryListId, mealScheduleId } ->
        [ S.link
            ( SpaceRoute state.spaceId $ GroceriesRoute groceryListId
                $ GroceriesGenerate mealScheduleId
            )
            [ HH.text "Generate" ]
        , S.link
            ( SpaceRoute state.spaceId $ GroceriesRoute groceryListId AddGrocery
            )
            [ HH.text "Add" ]
        ]
    isLive =
      Lens.preview _readyStateS state
        <#> case _ of
          Open -> true
          _ -> false
        # Maybe.fromMaybe false
