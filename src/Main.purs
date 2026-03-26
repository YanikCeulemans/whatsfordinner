module Main where

import Prelude

import App.App as App
import Effect (Effect)
import Effect.Now as Date
import Flame as F
import Flame.Subscription as FS
import Web.DOM.Document as Doc
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window as Window

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
  initUrlText <- documentUrlText
  F.mount appMount appId $ App.app targetDate initUrlText
  FS.send appId (App.InitializeRequested appId)

