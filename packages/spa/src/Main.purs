module Spa.Main where

import Prelude

import Data.Array (fold)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Spa.App.Route as Router
import Spa.AppM as AppM
import Spa.Data.Route (Route)
import Spa.Data.Route as Route
import Spa.FFI.Navigation as Navigation
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget as ET
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window

pathAndSearch :: Effect String
pathAndSearch = do
  location <- HTML.window >>= Window.location
  pathname <- Location.pathname location
  search <- Location.search location
  pure $ fold [ pathname, search ]

onNavigate :: (Maybe Route -> Aff Unit) -> Event -> Effect Unit
onNavigate f event = do
  Navigation.intercept opts navigationEvent
  pure unit
  where
  navigationEvent = Navigation.fromEvent event
  opts =
    { handler: do
        navigationEvent.destination.url
          # Route.parse
          # f
    }

interceptNavigation :: (Maybe Route -> Aff Unit) -> Aff Unit
interceptNavigation f = liftEffect do
  listener <- ET.eventListener $ onNavigate f
  navigation <- Navigation.toEventTarget <$> Navigation.navigation
  ET.addEventListener (EventType "navigate") listener true navigation
  pure unit

main :: Effect Unit
main = launchAff_ $ do
  let
    component = H.hoist AppM.runAppM Router.component

  body <- HA.awaitBody
  pas <- liftEffect pathAndSearch
  halogenIO <- runUI component (Route.parse' pas) body
  interceptNavigation \route ->
    case route of
      Just r ->
        void $ halogenIO.query $ H.mkTell $ Router.Navigate r
      Nothing -> pure unit
