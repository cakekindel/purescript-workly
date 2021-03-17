module Effect.Worker (Worker, sendMsg, onMsg) where

import Prelude

import Data.Newtype (class Newtype)
import Web.Event.Message (MessageEvent)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried ( EffectFn1
                        , EffectFn2
                        , EffectFn3
                        , runEffectFn1
                        , runEffectFn2
                        , runEffectFn3
                        )

--| Type binding for the `Worker` class, parameterized over
--| the request and response object types.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker)
foreign import data Worker :: Type -> Type -> Type
instance showWorker :: Show (Worker up dn) where show _ = "Worker"

--| Low-level binding for `Worker#postMessage`
--|
--| This will cause a listener attached in the child module
--| (by way of `Worker.Child.onMsg`) to fire with this message.
--|
--| If possible, it is recommended to use `Effect.Aff.Worker.Channel`
--| instead of this low-level API.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker)
sendMsg :: ∀ req res. Worker req res -> req -> Effect Unit
sendMsg = runEffectFn2 sendMsg_

--| Low-level binding for attaching a listener to `Worker#onmessage`.
--|
--| Any previously attached listener will be removed.
--|
--| If possible, it is recommended to use `Effect.Aff.Worker.Channel`
--| instead of this low-level API.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker/onmessage)
onMsg :: ∀ req res. Worker req res -> (MessageEvent res -> Effect Unit) -> Effect Unit
onMsg = runEffectFn3 onMsg_ $ unsafePerformEffect

foreign import sendMsg_ :: ∀ req res.   EffectFn2 (Worker req res) req Unit
foreign import onMsg_   :: ∀ a req res. EffectFn3 (Effect a -> a) (Worker req res) (MessageEvent res -> Effect Unit) Unit
