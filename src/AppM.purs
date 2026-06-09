module AppM (AppM, runAppM) where

import Prelude

import Capabilities.Navigation (class Navigation)
import Capabilities.Resource.ManageGroceryList (class ManageGroceryList, SortedGrocery)
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
    list /\ newState = getOrInsert id createValue state.groceryLists
  MonadState.put $ state { groceryLists = newState }
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
  upsert s = s
    { groceryLists = Map.alter (map go) groceryListId s.groceryLists }

localStorageUpsertGroceries
  :: GroceryListId -> Array GroceryEntry -> AppM GroceryList
localStorageUpsertGroceries groceryListId groceries = do
  void $ localStorageUpsertGroceryList groceryListId
  AppM $ sequential $ for_ groceries $ parallel
    <<< runAppM
    <<< localStorageUpsertGrocery groceryListId
  MonadState.gets $
    Maybe.fromMaybe' crash <<< Map.lookup groceryListId <<< _.groceryLists
  where
  crash _ = unsafeCrashWith "could not find just upserted grocery list?!"

localStorageDeleteGroceries
  :: GroceryListId -> Array GroceryEntry -> AppM GroceryList
localStorageDeleteGroceries id groceriesToDelete = do
  list <- localStorageUpsertGroceryList id
  let
    updatedList = GroceryList.deleteGroceries groceriesToDelete list
  MonadState.modify_ $ modify updatedList
  pure updatedList
  where
  modify updatedList s = s
    { groceryLists = Map.insert id updatedList s.groceryLists }

localStorageSuggestGroceries :: String -> AppM (Array SortedGrocery)
localStorageSuggestGroceries suggestionBase = do
  liftAff $ Aff.delay $ Milliseconds 500.0
  state <- MonadState.gets _.groceries
  state
    # Array.filter descriptionMatches
    # pure
  where
  crash err = unsafeCrashWith $ "could not create regex due to: " <> err
  suggestionBaseRegex =
    StrRegex.regex ("^" <> suggestionBase) Flags.ignoreCase
      # Either.either crash identity
  descriptionMatches { description } =
    StrRegex.test suggestionBaseRegex description

localStorageUpdateGroceries
  :: GroceryListId -> (GroceryEntry -> GroceryEntry) -> AppM GroceryList
localStorageUpdateGroceries id f = do
  list <- localStorageUpsertGroceryList id
  let
    updatedList = map f list
  MonadState.modify_ $ modify updatedList
  pure updatedList
  where
  modify updatedList s = s
    { groceryLists = Map.insert id updatedList s.groceryLists }

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

type State =
  { groceryLists :: Map GroceryListId GroceryList
  , groceries :: Array SortedGrocery
  }

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
  -- TODO: implement new state deserialization and serialization
  serializedState <- Storage.getItem "AppState" storage
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
