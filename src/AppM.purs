module AppM where

import Prelude

import Capabilities.Resource.ManageGroceryList (class ManageGroceryList)
import Data.Codec as Codec
import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Data.Either as Either
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Traversable (traverse)
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
  :: forall m. MonadAff m => String -> (Maybe String -> m String) -> m Unit
withStorageItem key mf = do
  storage <- liftEffect $ Window.localStorage =<< HTML.window
  item <- liftEffect $ Storage.getItem key storage
  updatedItem <- mf item
  liftEffect $ Storage.setItem key updatedItem storage

withCodec
  :: forall m a. Codec.Codec' m String a -> (m a -> a) -> String -> String
withCodec = ?h

decodeGroceryList :: String -> Either String GroceryList
decodeGroceryList candidate =
  AP.jsonParser candidate >>=
    ( CA.decode GroceryList.codec >>> lmap
        CA.printJsonDecodeError
    )

encodeGroceryList :: GroceryList -> String
encodeGroceryList = CA.encode GroceryList.codec >>> A.stringify

localStorageUpsertGrocery :: GroceryListId -> Grocery -> Aff Unit
localStorageUpsertGrocery id grocery = liftEffect do
  storage <- Window.localStorage =<< HTML.window
  serializedGroceryList <- Storage.getItem printedId storage
  let
    decodedList =
      serializedGroceryList
        # traverse decodeGroceryList
        # Either.hush
        # join
        # Maybe.fromMaybe mempty

    upsertedGroceryList = GroceryList.upsertGrocery grocery decodedList
    encodedList = encodeGroceryList upsertedGroceryList

  Storage.setItem printedId encodedList storage
  where
  printedId = GroceryListId.print id

instance ManageGroceryList AppM where
  upsertGrocery :: GroceryListId -> Grocery -> AppM Unit
  upsertGrocery id grocery = AppM $ localStorageUpsertGrocery id grocery
