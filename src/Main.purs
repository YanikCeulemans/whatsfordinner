module Main where

import Prelude

import App.App as App
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now as Date
import FFI.Navigation as FFINav
import Flame as F
import Flame.Subscription as FS
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget as ET

handleOnNavigate :: F.AppId String App.Message -> Event -> Effect Unit
handleOnNavigate appId evt =
  FFINav.intercept opts navEvt
  where
  navEvt = FFINav.fromEvent evt
  opts =
    { handler: do
        liftEffect $ FS.send appId $ App.NavigationOccurred
          navEvt.destination.url
    }

main :: Effect Unit
main = do
  let
    appId = F.AppId "whatsfordinner"
  targetDate <- Date.nowDate
  onNavigate <- ET.eventListener $ handleOnNavigate appId
  navigation <- FFINav.toEventTarget <$> FFINav.navigation
  ET.addEventListener (EventType "navigate") onNavigate true navigation
  F.mount (QuerySelector "#app") appId $ App.app targetDate

