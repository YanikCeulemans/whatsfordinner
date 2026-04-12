module AppM (AppM, runAppM) where

import Prelude

import Capabilities.Navigation (class Navigation)
import Capabilities.Resource.ManageGroceryList (class ManageGroceryList)
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Data.Either as Either
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Route (Route)
import Data.Route as Route
import Data.Traversable (traverse)
import Domain.Grocery (Grocery)
import Domain.GroceryList (GroceryList)
import Domain.GroceryList as GroceryList
import Domain.GroceryListId (GroceryListId)
import Domain.GroceryListId as GroceryListId
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import FFI.Navigation as Nav
import Web.HTML as HTML
import Web.HTML.Window as Window
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

withStorageItem
  :: forall m. MonadAff m => String -> (Maybe String -> String) -> m Unit
withStorageItem key mf = liftEffect do
  storage <- Window.localStorage =<< HTML.window
  item <- Storage.getItem key storage
  let
    updatedItem = mf item
  Storage.setItem key updatedItem storage

decodeGroceryList :: String -> Either String GroceryList
decodeGroceryList candidate =
  AP.jsonParser candidate >>=
    ( CA.decode GroceryList.codec >>> lmap
        CA.printJsonDecodeError
    )

encodeGroceryList :: GroceryList -> String
encodeGroceryList = CA.encode GroceryList.codec >>> A.stringify

upsertGrocery :: Grocery -> Maybe String -> String
upsertGrocery grocery serializedGroceryList =
  encodeGroceryList upsertedGroceryList
  where
  decodedList =
    serializedGroceryList
      # traverse decodeGroceryList
      # Either.hush
      # join
      # Maybe.fromMaybe mempty

  upsertedGroceryList = GroceryList.upsertGrocery grocery decodedList

localStorageUpsertGroceryList :: GroceryListId -> Aff GroceryList
localStorageUpsertGroceryList id = liftEffect do
  storage <- Window.localStorage =<< HTML.window
  serializedGroceryList <- Storage.getItem printedId storage
  let
    decodedList =
      serializedGroceryList
        # traverse decodeGroceryList
        # Either.hush
        # join

  Maybe.maybe (saveEmptyList storage) pure decodedList
  where
  printedId = GroceryListId.print id
  emptyGroceryList = mempty
  saveEmptyList storage =
    Storage.setItem printedId (encodeGroceryList emptyGroceryList) storage $>
      emptyGroceryList

localStorageUpsertGrocery :: GroceryListId -> Grocery -> Aff Unit
localStorageUpsertGrocery id grocery = do
  Aff.delay $ Milliseconds 500.0
  withStorageItem printedId $ upsertGrocery grocery
  where
  printedId = GroceryListId.print id

instance ManageGroceryList AppM where
  upsertGroceryList id = AppM $ localStorageUpsertGroceryList id
  upsertGrocery id grocery = AppM $ localStorageUpsertGrocery id grocery

setLocation :: Route -> Aff Unit
setLocation route = do
  navigation <- liftEffect Nav.navigation
  Nav.navigate printedRoute navigation
  where
  printedRoute = Route.print route

instance Navigation AppM where
  navigate route = AppM $ setLocation route
