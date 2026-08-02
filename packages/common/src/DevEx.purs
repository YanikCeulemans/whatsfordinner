module Common.DevEx where

import Prelude

import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Warn, Text)

todo :: forall a. Warn (Text "TODO left in the code") => String -> a
todo s = unsafeCrashWith $ "TODO: " <> s

