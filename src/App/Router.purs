module App.Route where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Route (Route)
import Data.Route as Route
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

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
    , render: HH.fromPlainHTML <<< render
    , eval:
        H.mkEval $ H.defaultEval
          { handleAction = handleAction
          , handleQuery = handleQuery
          , initialize = Just Initialize
          }
    }
  where
  initialState route = { route }

  -- TODO: add next 7 days as child when route is home
  render :: State -> PlainHTML
  render { route } =
    HH.div_
      [ HH.code_ [ HH.text $ show $ Route.print <$> route ]
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

