module Spa.AppM (AppM, runAppM) where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.State as MonadState
import Control.Parallel.Class (parallel, sequential)
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Either as Either
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List ((:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap)
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.String.Regex as StrRegex
import Data.String.Regex.Flags as Flags
import Data.Time.Duration (Seconds(..), convertDuration)
import Data.Traversable (for_)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Partial.Unsafe (unsafeCrashWith)
import Spa.App.Data as Data
import Spa.Capabilities.Navigation (class Navigation)
import Spa.Capabilities.Resource.ManageGroceryList
  ( class ManageGroceryList
  , SortedGrocery
  )
import Spa.Capabilities.Resource.ManageMealSchedule (class ManageMealSchedule)
import Spa.Capabilities.Resource.ManageSpaces (class ManageSpaces)
import Spa.Data.Route (Route)
import Spa.Data.Route as Route
import Spa.Data.ULID as DULID
import Common.GroceryList (GroceryEntry, GroceryList)
import Common.GroceryList as GroceryList
import Common.GroceryListId (GroceryListId)
import Common.Id as Id
import Common.MealScheduleId as MealScheduleId
import Spa.FFI.Navigation as Nav
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
  entryDescription = GroceryList.entryDescription grocery
  entrySortIndex = GroceryList.entrySortIndex grocery
  go = GroceryList.upsertEntry grocery
  upsert s = s
    { groceryLists = Map.alter (map go) groceryListId s.groceryLists
    , groceries = Map.insert (wrap entryDescription) entrySortIndex s.groceries
    }

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
    # Map.filterKeys (unwrap >>> textMatchesSuggestion)
    # foldlWithIndex reduce List.Nil
    # Array.fromFoldable
    # pure
  where
  crash err = unsafeCrashWith $ "could not create regex due to: " <> err
  suggestionBaseRegex =
    StrRegex.regex ("^" <> suggestionBase) Flags.ignoreCase
      # Either.either crash identity
  textMatchesSuggestion text =
    StrRegex.test suggestionBaseRegex text
  reduce key acc sortIndex =
    { description: unwrap key, sortIndex } : acc

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

type GroceryDescription = CaseInsensitiveString

groceryDescriptionCodec :: CA.JsonCodec GroceryDescription
groceryDescriptionCodec = dimap unwrap wrap CA.string

type SortIndex = Int

type State =
  { groceryLists :: Map GroceryListId GroceryList
  -- TODO: should be moved into the groceryLists map as added groceries are grocerylist specific?
  , groceries :: Map GroceryDescription SortIndex
  }

emptyState :: State
emptyState =
  { groceryLists: Map.empty
  , groceries: Map.empty
  }

stateCodec :: CA.JsonCodec State
stateCodec =
  CAR.object "State"
    { groceryLists: CAC.map Id.codec GroceryList.codec
    , groceries: CAC.map groceryDescriptionCodec CA.int
    }

decodeState :: String -> Either CA.JsonDecodeError State
decodeState candidate =
  AP.jsonParser candidate
    # lmap CA.TypeMismatch
    >>= CA.decode stateCodec

encodeState :: State -> String
encodeState = CA.encode stateCodec >>> A.stringify

deserializeState :: Maybe String -> Either String State
deserializeState = case _ of
  Nothing -> Left "no saved state found"
  Just candidate ->
    decodeState candidate # lmap toReadableError
  where
  toReadableError decodeErr =
    intercalate ": "
      [ "could not parse saved state due to error"
      , CA.printJsonDecodeError decodeErr
      ]

reportError :: forall m. MonadEffect m => Either String State -> m State
reportError = case _ of
  Left err -> do
    Console.log $
      "could not load state from local storage due to error: " <> err
    pure emptyState

  Right s -> pure s

loadState :: forall m. MonadEffect m => String -> Storage -> m State
loadState key storage = do
  deserializedState <- liftEffect $ Storage.getItem key storage
  reportError $ deserializeState deserializedState

saveState :: forall m. MonadEffect m => String -> Storage -> State -> m Unit
saveState key storage state =
  liftEffect $ Storage.setItem key serializedState storage
  where
  serializedState = encodeState state

stateKey :: String
stateKey = "AppState"

localStorageState :: forall a. (State -> Tuple a State) -> Aff a
localStorageState f = liftEffect do
  storage <- Window.localStorage =<< HTML.window
  state <- loadState stateKey storage

  let
    result /\ updatedState = f state

  saveState stateKey storage updatedState
  pure result

instance MonadState State AppM where
  state f = AppM $ localStorageState f

mkId :: forall a. String -> Id.Id a
mkId = DULID.parse >>> map Id.MkId >>> Either.fromRight' crash
  where
  crash _ = unsafeCrashWith "invalid hardcoded ULID"

instance ManageSpaces AppM where
  -- TODO: implement
  loadSpaces = pure
    [ { id: mkId "01KNW48VB0PNCFC0KZ8SW289ZA"
      , name: NonEmptyString "Komishes"
      , groceryListId: mkId "01KNW48VB0PNCFC0KZ8SW289ZZ"
      , mealScheduleId: MealScheduleId.MkMealScheduleId $ mkId
          "01KNW48VB0PNCFC0KZ8SW289ZZ"
      }
    ]

  loadSpace id = do
    liftAff $ Aff.delay $ convertDuration $ Seconds 3.0
    pure $ case Id.print id of
      "01KNW48VB0PNCFC0KZ8SW289ZA" ->
        Just
          { id: mkId "01KNW48VB0PNCFC0KZ8SW289ZA"
          , name: NonEmptyString "Fake space"
          , groceryListId: mkId "01KNW48VB0PNCFC0KZ8SW289ZZ"
          , mealScheduleId: MealScheduleId.MkMealScheduleId $ mkId
              "01KNW48VB0PNCFC0KZ8SW289ZZ"
          }
      _ -> Nothing

instance ManageMealSchedule AppM where
  -- TODO: Implement
  loadMealSchedule _id = do
    liftAff $ Aff.delay $ convertDuration $ Seconds 2.0
    pure $ Just Data.mealSchedule
