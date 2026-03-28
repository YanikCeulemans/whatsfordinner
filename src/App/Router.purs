module App.Route where

import Prelude

import App.Layout as Layout
import App.Next7Days as Next7Days
import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Data.Route (Route)
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slots = (next7Days :: forall query. H.Slot query Void Int)

_next7Days = Proxy :: Proxy "next7Days"

data Query a = Navigate Route a

type Input = State

type State =
  { route :: Maybe Route
  , currentDate :: Date
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
  initialState input = input

  -- TODO: add next 7 days as child when route is home
  render :: forall action. State -> H.ComponentHTML action Slots m
  render { route, currentDate } =
    Layout.main $ HH.div_
      [ HH.code_ [ HH.text $ show $ Route.print <$> route ]
      , case route of
          Just Route.Home -> HH.slot_ _next7Days 0 Next7Days.component ?h
          Nothing -> HH.h1_ [ HH.text "Not found" ]
          _ -> HH.text "TODO"
      , HH.a [ HP.href $ Route.print Route.Home ] [ HH.text "Home" ]
      , HH.a [ HP.href $ Route.print Route.Groceries ] [ HH.text "Groceries" ]
      ]

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

