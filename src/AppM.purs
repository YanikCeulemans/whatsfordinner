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
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
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
  :: forall m a
   . MonadAff m
  => String
  -> (Maybe String -> Tuple a String)
  -> m a
withStorageItem key mf = liftEffect do
  storage <- Window.localStorage =<< HTML.window
  item <- Storage.getItem key storage
  let
    updatedItem /\ encodedUpdatedItem = mf item
  Storage.setItem key encodedUpdatedItem storage
  pure updatedItem

decodeGroceryList :: String -> Either String GroceryList
decodeGroceryList candidate =
  AP.jsonParser candidate >>=
    ( CA.decode GroceryList.codec >>> lmap
        CA.printJsonDecodeError
    )

encodeGroceryList :: GroceryList -> String
encodeGroceryList = CA.encode GroceryList.codec >>> A.stringify

withGroceryList
  :: (GroceryList -> GroceryList) -> Maybe String -> Tuple GroceryList String
withGroceryList f serializedGroceryList =
  groceryList /\ encodeGroceryList groceryList
  where
  groceryList =
    serializedGroceryList
      # traverse decodeGroceryList
      # Either.hush
      # join
      # Maybe.fromMaybe mempty
      # f

localStorageUpsertGroceryList :: GroceryListId -> Aff GroceryList
localStorageUpsertGroceryList id =
  withStorageItem printedId $ withGroceryList identity
  where
  printedId = GroceryListId.print id

localStorageUpsertGrocery :: GroceryListId -> Grocery -> Aff Unit
localStorageUpsertGrocery id grocery = do
  Aff.delay $ Milliseconds 500.0
  void
    $ withStorageItem printedId
    $ withGroceryList
    $ GroceryList.upsertGrocery grocery
  where
  printedId = GroceryListId.print id

localStorageDeleteGroceries :: GroceryListId -> Array Grocery -> Aff GroceryList
localStorageDeleteGroceries id groceriesToDelete = do
  withStorageItem printedId
    $ withGroceryList
    $ GroceryList.deleteGroceries groceriesToDelete
  where
  printedId = GroceryListId.print id

localStorageUpdateGroceries
  :: GroceryListId -> (Grocery -> Grocery) -> Aff GroceryList
localStorageUpdateGroceries id f =
  withStorageItem printedId
    $ withGroceryList
    $ GroceryList.updateGroceries f
  where
  printedId = GroceryListId.print id

instance ManageGroceryList AppM where
  upsertGroceryList id = AppM $ localStorageUpsertGroceryList id
  upsertGrocery id grocery = AppM $ localStorageUpsertGrocery id grocery
  deleteGroceries id groceries = AppM $ localStorageDeleteGroceries id groceries
  updateGroceries id f = AppM $ localStorageUpdateGroceries id f

setLocation :: Route -> Aff Unit
setLocation route = do
  navigation <- liftEffect Nav.navigation
  Nav.navigate printedRoute navigation
  where
  printedRoute = Route.print route

instance Navigation AppM where
  navigate route = AppM $ setLocation route
