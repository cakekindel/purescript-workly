--| Module defining the `Worker` type and
--| a low-level API for interacting with worker threads.
module Effect.Worker (Worker, sendMsg, onMsg) where

import Prelude

import Data.Newtype (class Newtype)
import Web.Event.Message (MessageEvent)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried ( EffectFn2
                        , EffectFn3
                        , runEffectFn2
                        , runEffectFn3
                        )

fn2 = runEffectFn2
fn3 = runEffectFn3

--| Type binding for the `Worker` class, parameterized over
--| the request (`up`) and response (`dn`) object types.
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
--|
--| ```purescript
--| import Effect.Worker (Worker, sendMsg)
--|
--| worker :: Worker String _
--|
--| sendMsg "hello" worker
--| ```
sendMsg :: ∀ up dn. Worker up dn -> up -> Effect Unit
sendMsg = fn2 sendMsg_

--| Low-level binding for attaching a listener to `Worker#onmessage`.
--|
--| Any previously attached listener will be removed.
--|
--| If possible, it is recommended to use `Effect.Aff.Worker.Channel`
--| instead of this low-level API.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Worker/onmessage)
--|
--| ```purescript
--| import Effect.Worker (Worker, onMsg)
--| import Effect.Console (log)
--|
--| worker :: Worker _ String
--|
--| onMsg (\m -> log m) worker
--| ```
onMsg :: ∀ up dn. Worker up dn -> (MessageEvent dn -> Effect Unit) -> Effect Unit
onMsg = fn3 onMsg_ $ unsafePerformEffect

foreign import sendMsg_ :: ∀ up dn.   EffectFn2 (Worker up dn) up Unit
foreign import onMsg_   :: ∀ a up dn. EffectFn3 (Effect a -> a) (Worker up dn) (MessageEvent dn -> Effect Unit) Unit
