module AppM where

import Prelude

import Capabilities.Resource.ManageGroceryList (class ManageGroceryList)
import Data.Argonaut as A
import Data.Argonaut.Decode.Parser as CAP
import Data.Argonaut.Parser as AP
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Data.Either as Either
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Profunctor (lcmap)
import Data.Traversable (for, traverse)
import Domain.Grocery (Grocery)
import Domain.GroceryList (GroceryList)
import Domain.GroceryList as GroceryList
import Domain.GroceryListId (GroceryListId)
import Domain.GroceryListId as GroceryListId
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
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

localStorageUpsertGrocery :: GroceryListId -> Grocery -> Aff Unit
localStorageUpsertGrocery id grocery = do
  withStorageItem printedId $ upsertGrocery grocery
  where
  printedId = GroceryListId.print id

instance ManageGroceryList AppM where
  upsertGrocery :: GroceryListId -> Grocery -> AppM Unit
  upsertGrocery id grocery = AppM $ localStorageUpsertGrocery id grocery
