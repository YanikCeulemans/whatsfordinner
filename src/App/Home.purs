module App.Home where

import Prelude

import App.Layout as Layout
import Capabilities.Resource.ManageSpaces (class ManageSpaces)
import Capabilities.Resource.ManageSpaces as ManageSpaces
import Data.Maybe (Maybe(..))
import Domain.Space (Space)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type InitializedState =
  { spaces :: Array Space
  }

data State
  = NotInitialized
  | Initialized InitializedState

data Action = Initialize

component
  :: forall query input output m
   . MonadAff m
  => ManageSpaces m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render: render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize, handleAction = handleAction }
    }

  where
  initialState :: input -> State
  initialState _ = NotInitialized

  handleAction
    :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> do
      spaces <- ManageSpaces.loadSpaces
      H.modify_ \_ -> Initialized { spaces }

  render :: State -> H.ComponentHTML Action () m
  render state =
    Layout.main $
      case state of
        NotInitialized -> HH.p_ [ HH.text "loading" ]
        Initialized initializedState ->
          HH.div [ HP.class_ $ H.ClassName "flex column spaced" ]
            [ HH.h1_ [ HH.text "What's for dinner" ]
            , HH.text "TODO"
            ]

