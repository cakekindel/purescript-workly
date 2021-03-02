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
instance showWorker :: Show (Worker req res) where
  show _ = "Worker"

--| Type of an ECMAScript module containing a `default`
--| export that is a Worker execution body.
foreign import data Module :: Type -> Type -> Type

--|
spawn :: ∀ req res. Module req res -> Effect (Worker req res)
spawn = runEffectFn1 spawn_

--| Internal binding for `Worker#postMessage`
--|
--| This will cause a listener attached in the child module
--| (by way of `Worker.Child.onMsg`) to fire with this message.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker)
sendMsg :: ∀ req. Worker req _ -> req -> Effect Unit
sendMsg = runEffectFn2 sendMsg_

--| Internal binding for attaching a listener to `Worker#onmessage`.
--|
--| Any previously attached listener will be removed.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker/onmessage)
onMsg :: ∀ res. Worker _ res -> (MessageEvent res -> Effect Unit) -> Effect Unit
onMsg = runEffectFn3 onMsg_ $ unsafePerformEffect

foreign import spawn_   :: ∀ req res. EffectFn1 (Module req res) (Worker req res)
foreign import sendMsg_ :: ∀ req. EffectFn2 (Worker req _) req Unit
foreign import onMsg_   :: ∀ a res. EffectFn3 (Effect a -> a) (Worker _ res) (MessageEvent res -> Effect Unit) Unit
