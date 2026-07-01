module App.Route where

import Prelude

import App.AddGrocery as AddGrocery
import App.GenerateGroceries as GenerateGroceries
import App.Groceries as Groceries
import App.Home as Home
import App.Schedule as Schedule
import Capabilities.Navigation (class Navigation)
import Capabilities.Resource.ManageGroceryList (class ManageGroceryList)
import Capabilities.Resource.ManageSpaces (class ManageSpaces)
import Data.Maybe (Maybe(..))
import Data.Route (GroceryListInnerRoute(..), Route(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slots =
  ( home :: forall query. H.Slot query Void Int
  , schedule :: forall query. H.Slot query Void Int
  , groceries :: forall query. H.Slot query Void Int
  , generateGroceries :: forall query. H.Slot query Void Int
  , addGrocery :: forall query. H.Slot query Void Int
  )

_home = Proxy :: Proxy "home"
_schedule = Proxy :: Proxy "schedule"
_groceries = Proxy :: Proxy "groceries"
_addGrocery = Proxy :: Proxy "addGrocery"
_generateGroceries = Proxy :: Proxy "generateGroceries"

data Query a = Navigate Route a

type Input = Maybe Route

type State =
  { route :: Maybe Route
  }

data Action = Initialize

component
  :: forall output m
   . MonadAff m
  => ManageGroceryList m
  => ManageSpaces m
  => Navigation m
  => H.Component Query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval $ H.defaultEval
          { handleAction = handleAction
          , handleQuery = handleQuery
          , initialize = Just Initialize
          }
    }
  where
  initialState route = { route }

  render :: forall action. State -> H.ComponentHTML action Slots m
  render { route } =
    case route of
      Just Home -> HH.slot_ _home 0 Home.component unit
      Just Schedule -> HH.slot_ _schedule 0 Schedule.component unit
      Just
        (GroceryListRoute { groceryListId, groceryListRoute }) ->
        case groceryListRoute of
          Groceries -> HH.slot_ _groceries 0 Groceries.component groceryListId
          GroceriesGenerate ->
            HH.slot_ _generateGroceries 0 GenerateGroceries.component
              groceryListId
          AddGrocery -> HH.slot_ _addGrocery 0 AddGrocery.component
            groceryListId
      Nothing -> HH.h1_ [ HH.text "Not found" ]

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize ->
      pure unit

  handleQuery
    :: forall a childSlots
     . Query a
    -> H.HalogenM State Action childSlots output m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      when (route /= Just dest) do
        H.modify_ _ { route = Just dest }
      pure $ Just a

