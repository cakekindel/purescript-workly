module Effect.Worker where

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

--| Type of an ECMAScript module containing a `default`
--| export that is a Worker execution body.
foreign import data Module :: Type -> Type -> Type

--| Internal binding for `Worker#postMessage`
--|
--| This will cause a listener attached in the child module
--| (by way of `Worker.Child.onMsg`) to fire with this message.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker)
sendMsg :: ∀ req res. Worker req res -> req -> Effect Unit
sendMsg = runEffectFn2 sendMsg_

--| Internal binding for attaching a listener to `Worker#onmessage`.
--|
--| Any previously attached listener will be removed.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker/onmessage)
onMsg :: ∀ req res. Worker req res -> (MessageEvent res -> Effect Unit) -> Effect Unit
onMsg = runEffectFn3 onMsg_ $ unsafePerformEffect

foreign import spawn_   :: ∀ req res.   EffectFn1 (Module req res) (Worker req res)
foreign import sendMsg_ :: ∀ req res.   EffectFn2 (Worker req res) req Unit
foreign import onMsg_   :: ∀ a req res. EffectFn3 (Effect a -> a) (Worker req res) (MessageEvent res -> Effect Unit) Unit
