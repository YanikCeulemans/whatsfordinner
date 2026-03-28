module App.Route where

import Prelude

import App.Groceries as Groceries
import App.Next7Days as Next7Days
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slots =
  ( next7Days :: forall query. H.Slot query Void Int
  , groceries :: forall query. H.Slot query Void Int
  )

_next7Days = Proxy :: Proxy "next7Days"
_groceries = Proxy :: Proxy "groceries"

data Query a = Navigate Route a

type Input = Maybe Route

type State =
  { route :: Maybe Route
  }

data Action = Initialize

component
  :: forall output m. MonadAff m => H.Component Query Input output m
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
      Just Home -> HH.slot_ _next7Days 0 Next7Days.component unit
      Just Groceries -> HH.slot_ _groceries 0 Groceries.component unit
      Just GroceriesGenerate -> HH.text "TODO"
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

