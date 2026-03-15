module Main where

import Prelude

import App.App as App
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now as Date
import FFI.Navigation as FFINav
import Flame as F
import Flame.Subscription as FS
import Web.DOM.Document as Doc
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget as ET
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window as Window

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

documentUrlText :: Effect String
documentUrlText = do
  doc <- Window.document =<< HTML.window
  Doc.url $ HTMLDoc.toDocument doc

main :: Effect Unit
main = do
  let
    appId = F.AppId "whatsfordinner"
    appMount = QuerySelector "#app"
  targetDate <- Date.nowDate
  onNavigate <- ET.eventListener $ handleOnNavigate appId
  navigation <- FFINav.toEventTarget <$> FFINav.navigation
  ET.addEventListener (EventType "navigate") onNavigate true navigation
  initUrlText <- documentUrlText
  F.mount appMount appId $ App.app targetDate initUrlText

