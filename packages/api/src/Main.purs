module Api.Main where

import HTTPurple
import Prelude hiding ((/))

import Api.AppM (runAppM)
import Api.HTML (HTML)
import Api.HTML as AH
import Api.JsonBody as JsonBody
import Api.ManageMealSchedule (class ManageMealSchedule, loadMealSchedule)
import Api.ManageSpaces (class ManageSpaces, loadSpace, upsertSpace)
import Api.WS (WebSocket, WebSocketServer)
import Api.WS as WS
import Common.GroceryListId (GroceryListId)
import Common.Id (Id)
import Common.Id as Id
import Common.MealSchedule as MealSchedule
import Common.MealScheduleId (MealScheduleId)
import Common.MealScheduleId as MealScheduleId
import Common.Space as Space
import Common.SpaceId (SpaceId)
import Data.Argonaut (parseJson)
import Data.Argonaut as Argonaut
import Data.Array ((..))
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as JsonCodec
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Debug as Debug
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import HTTPurple.Body (RequestBody)
import HTTPurple.Body as RequestBody
import Node.Buffer (Buffer)
import Node.EventEmitter as EventEmitter
import Node.HTTP.IncomingMessage as IncomingMessage
import Node.HTTP.Types (IMServer, IncomingMessage)
import Node.Net.Socket as Socket
import Node.Net.Types (Socket, TCP)
import Node.Stream as Stream
import Simple.ULID (ULID)
import Simple.ULID as ULID
import Simple.ULID.Node as ULIDNode

data Route
  = Root
  | Api String
  | Spaces SpaceId
  | MealSchedule MealScheduleId

derive instance Generic Route _

spaceId' :: RouteDuplex' String -> RouteDuplex' SpaceId
spaceId' = as Id.print Id.parse

mealScheduleId' :: RouteDuplex' String -> RouteDuplex' MealScheduleId
mealScheduleId' = as MealScheduleId.print MealScheduleId.parse

route :: RouteDuplex' Route
route = mkRoute
  { "Root": noArgs
  , "Api": "api" / segment
  , "Spaces": "spaces" / spaceId' segment
  , "MealSchedule": "meal-schedules" / mealScheduleId' segment
  }

rootView :: Array ULID -> HTML
rootView ulids =
  AH.html []
    [ AH.head []
        [ AH.script []
            [ AH.text
                """
                let ws = null;
                const connect = () => {
                  if (ws) return;
                  ws = new WebSocket('/ws/01KYFT2TNZQ356N53Z1HV7JK7D');
                  ws.addEventListener('message', evt => {
                    console.log('message received', evt);
                  });
                  ws.addEventListener('close', evt => {
                    console.log('socket closed', evt);
                  });
                };
                const disconnect = () => {
                  console.log("disconnect from js");
                  ws?.close();
                  ws = null;
                };
                """
            ]
        ]
    , AH.body []
        [ AH.button [ "onclick='connect()'" ] [ AH.text "connect" ]
        , AH.button [ "onclick='disconnect()'" ] [ AH.text "disconnect" ]
        , AH.ul []
            ( ulids
                <#> ULID.toString >>> AH.text >>> pure >>> AH.li []
            )
        ]
    ]

groceryListIdDuplex :: RouteDuplex' String -> RouteDuplex' GroceryListId
groceryListIdDuplex = as Id.print Id.parse

data WSRoute = WS GroceryListId

derive instance Generic WSRoute _

wsRoute :: RouteDuplex' WSRoute
wsRoute = mkRoute { "WS": "ws" / groceryListIdDuplex segment }

onUpgrade
  :: Ref (Map GroceryListId (Map (Id WebSocket) WebSocket))
  -> WebSocketServer
  -> IncomingMessage IMServer
  -> Socket TCP
  -> Buffer
  -> Effect Unit
onUpgrade websockets wss request socket headBuffer = do
  case parse wsRoute $ IncomingMessage.url request of
    Right (WS groceryListId) ->
      wss # WS.handleUpgrade request socket headBuffer
        \ws -> do
          socketId <- Id.MkId <$> ULID.genULID ULIDNode.prng
          Ref.modify_ (Map.alter (insert socketId ws) groceryListId) websockets

          ws #
            ( EventEmitter.on_ WS.closeH $
                Ref.modify_
                  (Map.alter (remove socketId) groceryListId)
                  websockets
            )

          WS.emitConnection ws request wss
      where
      insert socketId ws = case _ of
        Nothing -> Just $ Map.singleton socketId ws
        Just others -> Just $ Map.insert socketId ws others
      remove socketId = case _ of
        Nothing -> Nothing
        Just sockets -> Just $ Map.delete socketId sockets

    Left _ ->
      Socket.toDuplex socket
        # Stream.destroy

findSpaceHandler
  :: forall m. MonadAff m => ManageSpaces m => SpaceId -> m Response
findSpaceHandler spaceId = do
  foundSpace <- loadSpace spaceId
  case foundSpace of
    Nothing -> noContent
    Just space -> ok $ JsonBody.create Space.spaceCodec space

findMealScheduleHandler
  :: forall m
   . MonadAff m
  => ManageMealSchedule m
  => MealScheduleId
  -> m Response
findMealScheduleHandler mealScheduleId = do
  foundMealSchedule <- loadMealSchedule mealScheduleId
  case foundMealSchedule of
    Nothing -> noContent
    Just mealSchedule -> ok $ JsonBody.create MealSchedule.codec mealSchedule

decodeBody
  :: forall a m
   . MonadAff m
  => JsonCodec a
  -> RequestBody
  -> m (Either String a)
decodeBody codec requestBody = do
  bodyString <- RequestBody.toString requestBody
  pure do
    json <- lmap Argonaut.printJsonDecodeError $ parseJson bodyString
    lmap JsonCodec.printJsonDecodeError $ JsonCodec.decode codec json

upsertSpaceHandler
  :: forall m
   . MonadAff m
  => ManageSpaces m
  => SpaceId
  -> RequestBody
  -> m Response
upsertSpaceHandler spaceId requestBody = do
  decodedSpace <- decodeBody Space.spaceCodec requestBody
  case decodedSpace of
    Left error -> badRequest $ JsonBody.create'
      { error: "BadRequest", details: error }
    Right space -> do
      upsertSpace spaceId space
      ok $ JsonBody.create Space.spaceCodec space

main :: Effect Unit
main = launchAff_ do
  env <- createEnv
  Aff.makeAff \_done -> do
    websockets <- Ref.new Map.empty
    wss <- WS.mkWebSocketServer { noServer: true }
    wss # EventEmitter.on_ WS.connectionH \ws -> do
      launchAff_ do
        Aff.delay $ Milliseconds 2500.0
        liftEffect $ WS.send "hello, world" ws
    void $ serve
      { port: 8080
      , onUpgrade: Just $ onUpgrade websockets wss
      }
      { route, router: router env }
    pure Aff.nonCanceler
  where
  createEnv = do
    spaces <- AVar.new $ Map.empty
    mealSchedules <- AVar.new $ Map.empty
    pure { spaces, mealSchedules }
  router appState = case _ of
    { route: Root } -> do
      ulids <- for (1 .. 5) $ const $ liftEffect $ ULID.genULID ULIDNode.prng
      ok $ rootView ulids

    { route: Api rest } -> ok $ "api route " <> rest

    { route: Spaces spaceId, method: Get } -> do
      runAppM appState $ findSpaceHandler spaceId
    { route: Spaces spaceId, method: Put, body: requestBody } -> do
      runAppM appState $ upsertSpaceHandler spaceId requestBody

    { route: MealSchedule mealScheduleId, method: Get } ->
      runAppM appState $ findMealScheduleHandler mealScheduleId

    _ -> notFound
