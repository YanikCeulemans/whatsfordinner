module Main where

import Prelude

import App.App as App
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Now as Date
import Flame as F
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = do
  targetDate <- Date.nowDate
  F.mount_ (QuerySelector "#app") $ App.app targetDate

