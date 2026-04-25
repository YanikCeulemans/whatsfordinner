module AppM (AppM, runAppM) where

import Prelude

import Capabilities.Navigation (class Navigation)
import Capabilities.Resource.ManageGroceryList (class ManageGroceryList)
import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Array (foldl, (..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Either as Either
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Route (Route)
import Data.Route as Route
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.ULID as DULID
import Domain.GroceryEntry (GroceryEntry)
import Domain.GroceryList (GroceryList)
import Domain.GroceryList as GroceryList
import Domain.GroceryListId (GroceryListId(..))
import Domain.GroceryListId as GroceryListId
import Domain.Id as Id
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import FFI.Navigation as Nav
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.Storage.Storage (Storage)
import Web.Storage.Storage as Storage

newtype AppM a = AppM (Aff a)

runAppM :: AppM ~> Aff
runAppM (AppM aff) = aff

derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM

-- TODO: Id types with phantom type instead of bespoke ids?

decodeGroceryList :: String -> Either String GroceryList
decodeGroceryList candidate =
  AP.jsonParser candidate >>=
    ( CA.decode GroceryList.codec >>> lmap
        CA.printJsonDecodeError
    )

encodeGroceryList :: GroceryList -> String
encodeGroceryList = CA.encode GroceryList.codec >>> A.stringify

getOrInsert
  :: forall k v. Ord k => k -> (Unit -> v) -> Map k v -> Tuple v (Map k v)
getOrInsert key createValue map =
  case Map.lookup key map of
    Just value -> value /\ map
    Nothing ->
      value /\ updatedMap
      where
      value = createValue unit
      updatedMap = Map.insert key value map

localStorageUpsertGroceryList :: GroceryListId -> AppM GroceryList
localStorageUpsertGroceryList id = do
  state <- MonadState.get
  let
    list /\ newState = getOrInsert id createValue state
  MonadState.put newState
  pure list
  where
  createValue _ = mempty

localStorageUpsertGrocery :: GroceryListId -> GroceryEntry -> AppM Unit
localStorageUpsertGrocery id grocery = do
  liftAff $ Aff.delay $ Milliseconds 300.0
  MonadState.modify_ upsert
  where
  go = GroceryList.upsertGrocery grocery
  upsert = Map.alter (map go) id

localStorageDeleteGroceries
  :: GroceryListId -> Array GroceryEntry -> AppM GroceryList
localStorageDeleteGroceries id groceriesToDelete = do
  list <- localStorageUpsertGroceryList id
  let
    updatedList = GroceryList.deleteGroceries groceriesToDelete list
  MonadState.modify_ (Map.insert id updatedList)
  pure updatedList

localStorageUpdateGroceries
  :: GroceryListId -> (GroceryEntry -> GroceryEntry) -> AppM GroceryList
localStorageUpdateGroceries id f = do
  list <- localStorageUpsertGroceryList id
  let
    updatedList = map f list
  MonadState.modify_ (Map.insert id updatedList)
  pure updatedList

instance ManageGroceryList AppM where
  upsertGroceryList id = localStorageUpsertGroceryList id
  upsertGrocery id grocery = localStorageUpsertGrocery id grocery
  deleteGroceries id groceries = localStorageDeleteGroceries id groceries
  updateGroceries id f = localStorageUpdateGroceries id f

setLocation :: Route -> Aff Unit
setLocation route = do
  navigation <- liftEffect Nav.navigation
  Nav.navigate printedRoute navigation
  where
  printedRoute = Route.print route

instance Navigation AppM where
  navigate route = AppM $ setLocation route

type State = Map GroceryListId GroceryList

groceryListIdKeys :: Array String -> Array GroceryListId
groceryListIdKeys xs =
  xs
    # map DULID.parse
    # keepRights
    # map Id.MkId
  where
  go acc = case _ of
    Left _ -> acc
    Right x -> acc <> [ x ]
  keepRights = foldl go []

getGroceryListStorageItem
  :: Storage
  -> GroceryListId
  -> Effect (Maybe (Tuple GroceryListId GroceryList))
getGroceryListStorageItem storage id = do
  foundItem <- Storage.getItem printedId storage
  let
    decoded =
      for foundItem decodeGroceryList
        # Either.hush
        # join
  pure $ map toTuple decoded
  where
  printedId = Id.print id
  toTuple list = Tuple id list

setGroceryListStorageItem
  :: Storage -> GroceryListId -> GroceryList -> Effect Unit
setGroceryListStorageItem storage id list = do
  Storage.setItem printedId encodedList storage
  where
  printedId = Id.print id
  encodedList = encodeGroceryList list

localStorageState :: forall a. (State -> Tuple a State) -> Aff a
localStorageState f = liftEffect do
  storage <- Window.localStorage =<< HTML.window
  length <- Storage.length storage
  keys <- for (0 .. length) (\i -> Storage.key i storage)

  let
    idKeys = groceryListIdKeys $ Array.catMaybes keys

  groceryListMaybes <- for idKeys $ getGroceryListStorageItem storage
  let
    groceryLists =
      groceryListMaybes
        # Array.catMaybes
        # Map.fromFoldable

    result /\ updatedGroceryLists = f groceryLists

  forWithIndex_ updatedGroceryLists $ setGroceryListStorageItem storage

  pure result

instance MonadState State AppM where
  state f = AppM $ localStorageState f
