module Api.AppM where

import Prelude

import Api.ManageGroceryList
  ( class ManageGroceryList
  , UpsertGroceryEntryError(..)
  )
import Api.ManageMealSchedule (class ManageMealSchedule)
import Api.ManageSpaces (class ManageSpaces)
import Api.WS (WebSocket)
import Api.WS as WS
import Common.DevEx as DevEx
import Common.GroceryList (GroceryEntry, GroceryList)
import Common.GroceryListId (GroceryListId)
import Common.Id (Id)
import Common.MealSchedule (MealSchedule)
import Common.MealScheduleId (MealScheduleId)
import Common.Space (Space)
import Common.SpaceId (SpaceId)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (for_)
import Effect.Aff (Aff, bracket, try)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref

type WebsocketMapRef = Ref (Map GroceryListId (Map (Id WebSocket) WebSocket))

type Env =
  ( { spaces :: AVar (Map SpaceId Space)
    , mealSchedules :: AVar (Map MealScheduleId MealSchedule)
    , groceryLists :: AVar (Map GroceryListId GroceryList)
    , websockets :: Ref (Map GroceryListId (Map (Id WebSocket) WebSocket))
    }
  )

newtype AppM a = MkAppM (ReaderT Env Aff a)

runAppM :: forall a. Env -> AppM a -> Aff a
runAppM state (MkAppM app) = runReaderT app state

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Bind AppM
derive newtype instance Applicative AppM
derive newtype instance Monad AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAsk Env AppM

loadSpaceFromMemory :: SpaceId -> AppM (Maybe Space)
loadSpaceFromMemory spaceId = do
  env <- ask
  spaces <- liftAff $ AVar.read env.spaces
  pure $ Map.lookup spaceId spaces

upsertSpaceFromMemory :: SpaceId -> Space -> AppM Unit
upsertSpaceFromMemory spaceId space = do
  env <- ask
  spaces <- liftAff $ AVar.take env.spaces
  let
    updatedSpaces = Map.insert spaceId space spaces
  liftAff $ AVar.put updatedSpaces env.spaces

loadMealScheduleFromMemory :: MealScheduleId -> AppM (Maybe MealSchedule)
loadMealScheduleFromMemory mealScheduleId = do
  env <- ask
  mealSchedules <- liftAff $ AVar.read env.mealSchedules
  pure $ Map.lookup mealScheduleId mealSchedules

loadGroceryListFromMemory :: GroceryListId -> AppM (Maybe GroceryList)
loadGroceryListFromMemory groceryListId = do
  env <- ask
  groceryLists <- liftAff $ AVar.read env.groceryLists
  pure $ Map.lookup groceryListId groceryLists

upsertGroceryListFromMemory :: GroceryListId -> GroceryList -> AppM Unit
upsertGroceryListFromMemory groceryListId groceryList = do
  env <- ask
  groceryLists <- liftAff $ AVar.take env.groceryLists
  let
    updatedGroceryLists = Map.insert groceryListId groceryList groceryLists
  liftAff $ AVar.put updatedGroceryLists env.groceryLists

refRead :: forall m a. MonadEffect m => Ref a -> m a
refRead = liftEffect <<< Ref.read

readGroceryListWebsocketsOrEmpty
  :: forall m
   . MonadEffect m
  => GroceryListId
  -> WebsocketMapRef
  -> m (Array WebSocket)
readGroceryListWebsocketsOrEmpty groceryListId ref = map help $ refRead ref
  where
  help = Map.lookup groceryListId
    >>> Maybe.fromMaybe Map.empty
    >>> Array.fromFoldable

upsertGroceryEntryFromMemory
  :: GroceryListId -> GroceryEntry -> AppM (Either UpsertGroceryEntryError Unit)
upsertGroceryEntryFromMemory groceryListId groceryEntry = do
  env <- ask
  groceryLists <- liftAff $ AVar.take env.groceryLists
  case Map.lookup groceryListId groceryLists of
    Nothing -> liftAff do
      AVar.put groceryLists env.groceryLists
      pure $ Left $ NoSuchGroceryList groceryListId

    Just groceryList -> liftAff do
      let
        updatedGroceryLists =
          groceryList
            # Array.filter (not <<< eq groceryEntry)
            # Array.cons groceryEntry
            # \x -> Map.insert groceryListId x groceryLists

      AVar.put updatedGroceryLists env.groceryLists

      groceryListWebsockets <- readGroceryListWebsocketsOrEmpty groceryListId
        env.websockets
      for_ groceryListWebsockets \websocket -> do
        pure $ DevEx.todo "send event over websocket"

      pure $ Right unit

instance ManageSpaces AppM where
  loadSpace = loadSpaceFromMemory
  upsertSpace = upsertSpaceFromMemory

instance ManageMealSchedule AppM where
  loadMealSchedule = loadMealScheduleFromMemory

instance ManageGroceryList AppM where
  loadGroceryList = loadGroceryListFromMemory
  upsertGroceryList = upsertGroceryListFromMemory
  upsertGroceryEntry = upsertGroceryEntryFromMemory
