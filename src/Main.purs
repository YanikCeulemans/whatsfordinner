module Main where

import Prelude

import App.Route as Router
import AppM as AppM
import Data.Array (fold)
import Data.Maybe (Maybe(..))
import Data.Route (Route)
import Data.Route as Route
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FFI.Navigation as Navigation
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.Document as Doc
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget as ET
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDoc
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
