module FFI.FFIDoc where

import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Aff.Compat (EffectFn1, runEffectFn1)
import Prelude ((<#>), (>>>))
import Web.HTML (HTMLDocument)
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState(..))
import Web.HTML.HTMLDocument.VisibilityState as VisibilityState

foreign import _visibilityStateImpl :: EffectFn1 HTMLDocument String

visibilityState :: HTMLDocument -> Effect VisibilityState
visibilityState doc =
  runEffectFn1 _visibilityStateImpl doc
    <#> VisibilityState.parse
      >>> Maybe.fromMaybe Visible
