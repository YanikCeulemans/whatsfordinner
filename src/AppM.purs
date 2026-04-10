module AppM where

import Prelude

import Capabilities.Resource.ManageGroceryList (class ManageGroceryList)
import Data.Argonaut as A
import Domain.Grocery (Grocery)
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

localStorageUpsertGrocery :: GroceryListId -> Grocery -> Aff Unit
localStorageUpsertGrocery id grocery = liftEffect do
  storage <- Window.localStorage =<< HTML.window
  serializedGroceryList <- Storage.getItem printedId storage
  pure unit
  where
  printedId = GroceryListId.print id

instance ManageGroceryList AppM where
  upsertGrocery :: GroceryListId -> Grocery -> AppM Unit
  upsertGrocery id grocery = AppM $ localStorageUpsertGrocery id grocery
