module FFI.Navigation where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn2, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventTarget)

foreign import data Navigation :: Type

toEventTarget :: Navigation -> EventTarget
toEventTarget = unsafeCoerce

foreign import navigation :: Effect Navigation

type NavigateEvent = { destination :: { url :: String } }

fromEvent :: Event -> NavigateEvent
fromEvent = unsafeCoerce

type InterceptOptionsImpl =
  { handler :: Effect (Promise Unit) }

foreign import _interceptImpl
  :: EffectFn2 InterceptOptionsImpl NavigateEvent Unit

type InterceptOptions = { handler :: Aff Unit }

toInterceptOptionsImpl :: InterceptOptions -> InterceptOptionsImpl
toInterceptOptionsImpl opts =
  { handler }
  where
  handler = Promise.fromAff opts.handler

intercept :: InterceptOptions -> NavigateEvent -> Effect Unit
intercept opts evt = do
  runEffectFn2 _interceptImpl optsImpl evt
  where
  optsImpl = toInterceptOptionsImpl opts

foreign import _navigateImpl :: EffectFn2 String Navigation (Promise Unit)

navigate :: String -> Navigation -> Aff Unit
navigate url nav = Promise.toAffE $ runEffectFn2 _navigateImpl url nav
