module AppM (AppM, runAppM) where

import Prelude

import Capabilities.Navigation (class Navigation)
import Capabilities.Resource.ManageGroceryList (class ManageGroceryList)
import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Control.Parallel.Class (parallel, sequential)
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
import Data.Maybe as Maybe
import Data.Route (Route)
import Data.Route as Route
import Data.Set as Set
import Data.String.Regex as StrRegex
import Data.String.Regex.Flags as Flags
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Data.ULID as DULID
import Domain.GroceryList (GroceryList, GroceryEntry)
import Domain.GroceryList as GroceryList
import Domain.GroceryListId (GroceryListId)
import Domain.Id as Id
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import FFI.Navigation as Nav
import Partial.Unsafe (unsafeCrashWith)
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
localStorageUpsertGrocery groceryListId grocery = do
  liftAff $ Aff.delay $ Milliseconds 300.0
  MonadState.modify_ upsert
  where
  entryId = GroceryList.entryId grocery
  entryDescription = GroceryList.entryDescription grocery
  entryAmount = GroceryList.entryAmount grocery
  go = GroceryList.upsertGrocery entryId entryDescription entryAmount >>> snd
  upsert = Map.alter (map go) groceryListId

localStorageUpsertGroceries
  :: GroceryListId -> Array GroceryEntry -> AppM GroceryList
localStorageUpsertGroceries groceryListId groceries = do
  void $ localStorageUpsertGroceryList groceryListId
  AppM $ sequential $ for_ groceries $ parallel
    <<< runAppM
    <<< localStorageUpsertGrocery groceryListId
  MonadState.gets $ Maybe.fromMaybe' crash <<< Map.lookup groceryListId
  where
  crash _ = unsafeCrashWith "could not find just upserted grocery list?!"

localStorageDeleteGroceries
  :: GroceryListId -> Array GroceryEntry -> AppM GroceryList
localStorageDeleteGroceries id groceriesToDelete = do
  list <- localStorageUpsertGroceryList id
  let
    updatedList = GroceryList.deleteGroceries groceriesToDelete list
  MonadState.modify_ (Map.insert id updatedList)
  pure updatedList

localStorageSuggestGroceries :: String -> AppM (Array String)
localStorageSuggestGroceries suggestionBase = do
  state <- MonadState.get
  Map.values state
    # Array.fromFoldable
    # join
    # map GroceryList.entryDescription
    # Set.fromFoldable
    # Array.fromFoldable
    # Array.filter (StrRegex.test suggestionBaseRegex)
    # pure
  where
  crash err = unsafeCrashWith $ "could not create regex due to: " <> err
  suggestionBaseRegex =
    StrRegex.regex suggestionBase Flags.ignoreCase
      # Either.either crash identity

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
  upsertGroceries id groceries = localStorageUpsertGroceries id groceries
  deleteGroceries id groceries = localStorageDeleteGroceries id groceries
  updateGroceries id f = localStorageUpdateGroceries id f
  suggestGroceries = localStorageSuggestGroceries

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
