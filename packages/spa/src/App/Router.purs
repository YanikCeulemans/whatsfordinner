module Spa.App.Route where

import Prelude

import Spa.App.AddGrocery as AddGrocery
import Spa.App.GenerateGroceries as GenerateGroceries
import Spa.App.Groceries as Groceries
import Spa.App.Home (HomeOutput(..))
import Spa.App.Home as Home
import Spa.App.Schedule as Schedule
import Spa.Capabilities.Navigation (class Navigation)
import Spa.Capabilities.Resource.ManageGroceryList (class ManageGroceryList)
import Spa.Capabilities.Resource.ManageMealSchedule (class ManageMealSchedule)
import Spa.Capabilities.Resource.ManageSpaces (class ManageSpaces)
import Data.Maybe (Maybe(..))
import Spa.Data.Route (GroceriesRoute(..), Route(..), SpaceRoute(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slots =
  ( home :: forall query. Home.HomeSlot query Int
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

data Action
  = Initialize
  | HandleHome HomeOutput

component
  :: forall output m
   . MonadAff m
  => ManageGroceryList m
  => ManageSpaces m
  => ManageMealSchedule m
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

  render :: State -> H.ComponentHTML Action Slots m
  render { route } =
    case route of
      Just Home -> HH.slot _home 0 Home.component unit HandleHome

      Just (SpaceRoute spaceId innerRoute) ->
        case innerRoute of
          Schedule -> HH.slot_ _schedule 0 Schedule.component spaceId

          GroceriesRoute groceryListId Groceries -> HH.slot_ _groceries 0
            Groceries.component
            { spaceId, groceryListId }

          GroceriesRoute groceryListId (GroceriesGenerate mealScheduleId) ->
            HH.slot_ _generateGroceries 0 GenerateGroceries.component
              { spaceId, groceryListId, mealScheduleId }

          GroceriesRoute groceryListId AddGrocery ->
            HH.slot_ _addGrocery 0 AddGrocery.component
              { groceryListId
              , routes:
                  { cancel: groceriesRoute
                  , submit: groceriesRoute
                  }
              }
            where
            groceriesRoute = SpaceRoute spaceId $
              GroceriesRoute groceryListId Groceries

      Nothing -> HH.h1_ [ HH.text "Not found" ]

  handleAction
    :: forall childSlots
     . Action
    -> H.HalogenM State Action childSlots output m Unit
  handleAction = case _ of
    Initialize ->
      pure unit

    HandleHome (InviteAccepted spaceId) -> do
      Console.logShow { msg: "invite accepted", spaceId }
      pure unit

    HandleHome (SpaceSelected space) -> do
      Console.logShow { msg: "space selected", name: space.name }
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
