module Main where

import Prelude

import App.App as App
import Effect (Effect)
import Flame as F
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main =
  F.mount_ (QuerySelector "body") App.app

