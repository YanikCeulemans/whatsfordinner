module Common.Greeting where

import Prelude

greeting :: String -> String
greeting subject =
  "Hello, " <> subject
