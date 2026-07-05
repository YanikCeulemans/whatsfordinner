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
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Route (Route(..), SpaceInnerRoute(..))
import Data.Time.Duration (Seconds(..), convertDuration)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Domain.Amount (Amount(..))
import Domain.GroceryList (GroceryEntry, GroceryList)
import Domain.GroceryList as GroceryList
import Domain.GroceryListId (GroceryListId)
import Domain.Id as Id
import Domain.SpaceId (SpaceId)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import FFI.WebSocket (WebSocket)
import FFI.WebSocket as WS
import FFI.WebSocket.Types.CloseEvent (CloseEvent)
import FFI.WebSocket.Types.CloseEvent as WSTC
import FFI.WebSocket.Types.MessageEvent (MessageEvent)
import FFI.WebSocket.Types.ReadyState (ReadyState(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
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

type Input = SpaceId

type State =
  { groceryList :: GroceryList
  , dragState :: DragState DragEntry
  , allowDragging :: Boolean
  , webSocketState :: Maybe WebSocketState
  , spaceId :: SpaceId
  }

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

endDrag :: State -> Tuple (Array GroceryEntry) State
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

setGroceryEntry :: GroceryEntry -> State -> State
setGroceryEntry grocery s =
  s { groceryList = GroceryList.set grocery s.groceryList }

clearCompleted :: State -> State
clearCompleted state = state
  { groceryList = GroceryList.clearCompleted state.groceryList }

uncheckCompleted :: State -> State
uncheckCompleted state = state
  { groceryList = map GroceryList.uncheckEntry state.groceryList }

data Action
  = Initialize
  | Finalize
  | ConnectWebSocket
  | ToggleGrocery GroceryEntry MouseEvent
  | StartDrag DragEntry DragEvent
  | OverDrag DragEntry DragEvent
  | EndDrag DragEvent
  | ClearCompleted
  | UncheckCompleted
  | HandleMouseDown
  | MessageReceived MessageEvent
  | WebSocketOpened
  | WebSocketClosed CloseEvent

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
  => State
  -> Tuple Int GroceryEntry
  -> H.ComponentHTML Action () m
groceryView { dragState, allowDragging } (Tuple index grocery) =
  let
    dragEntry = { index, item: grocery }
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
                  , HE.onMouseDown $ const HandleMouseDown
                  , HE.onTouchStart $ const HandleMouseDown
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
  => State
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
  state <- H.get
  readyState <- H.liftEffect $ traverse getReadyState state.webSocketState
  H.put $ state
    { webSocketState =
        setReadyState <$> readyState <*> state.webSocketState
    }
  where
  getReadyState { webSocket } = WS.readyState webSocket
  setReadyState readyState webSocketState = webSocketState
    { readyState = readyState }

component
  :: forall query output m
   . MonadAff m
  => ManageGroceryList m
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
  initialState spaceId =
    { groceryList: mempty
    , dragState: NotDragging
    , allowDragging: false
    , webSocketState: Nothing
    , spaceId
    }

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize -> do
      groceryList <- upsertGroceryList Data.dummyListId
      H.modify_ _ { groceryList = groceryList }
      handleAction ConnectWebSocket

    Finalize -> do
      webSocket <- map _.webSocket <$> H.gets _.webSocketState
      H.liftEffect $ for_ webSocket $ WS.close WSTC.NoLongerInterested

    -- TODO: WebSocket to capability?
    ConnectWebSocket -> do
      -- TODO: handle socket error event, try to reconnect?
      ws <- connectWebSocket Data.dummyListId
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
        (WebSocketClosed >>> Just)
        ws
      readyState <- H.liftEffect $ WS.readyState ws
      H.modify_ _ { webSocketState = Just { webSocket: ws, readyState } }

    WebSocketOpened -> do
      Console.log "web socket opened"
      updateReadyState

    WebSocketClosed closeEvent -> do
      Console.logShow { msg: "web socket closed", closeEvent }
      updateReadyState
      unless (isNoLongerInterested closeEvent) do
        Console.log "retrying web socket connection"
        H.liftAff $ Aff.delay $ convertDuration $ Seconds 5.0
        handleAction ConnectWebSocket

    StartDrag dragEntry _dragEvent ->
      H.modify_ $ _ { dragState = DraggingOverNonTarget dragEntry }

    OverDrag dragEntry dragEvent -> do
      preventDefault dragEvent
      H.modify_ setDragOver
      where
      setDragOver state = state
        { dragState = dragOverTarget dragEntry state.dragState }

    EndDrag _dragEvent -> do
      state <- H.get
      let
        modifiedGroceries /\ newState = endDrag state

      H.put $ newState { allowDragging = false }
      void $ updateGroceries Data.dummyListId $ syncWith modifiedGroceries

      where
      syncWith modifiedGroceries grocery =
        Array.find (eq grocery) modifiedGroceries
          # Maybe.fromMaybe grocery

    ToggleGrocery grocery mouseEvent -> do
      preventDefault mouseEvent
      H.modify_ $ setGroceryEntry toggledGrocery
      upsertGrocery Data.dummyListId toggledGrocery
      where
      toggledGrocery = GroceryList.toggleEntryChecked grocery

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
      void $ updateGroceries Data.dummyListId GroceryList.uncheckEntry

    HandleMouseDown ->
      H.modify_ _ { allowDragging = true }

    MessageReceived event ->
      Console.log $ fold [ "msg received: ", event.data ]

  render :: State -> H.ComponentHTML Action () m
  render state =
    Layout.main' (Layout.defaultMainConfig { spaceId = Just state.spaceId }) $
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
                            state.webSocketState
                        , "pico-background-pink-450": (not <<< isLive)
                            state.webSocketState
                        }
                    ]
                    []
                ]
            , HH.div [ HP.class_ $ H.ClassName "flex spaced" ]
                [ S.link
                    ( SpaceRoute
                        { spaceId: state.spaceId
                        , route: GroceriesGenerate
                        }
                    )
                    [ HH.text "Generate" ]
                , S.link
                    ( SpaceRoute
                        { spaceId: state.spaceId
                        , route: AddGrocery
                        }
                    )
                    [ HH.text "Add" ]
                ]
            ]
        , case state.groceryList of
            [] -> HH.text "No groceries have been added yet"
            _ -> groceriesView state
        ]
    where
    isLive ws =
      ws
        <#> _.readyState
        <#> case _ of
          Open -> true
          _ -> false
        # Maybe.fromMaybe false
