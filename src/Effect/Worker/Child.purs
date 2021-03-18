--| Module defining the `Worker` type and
--| a low-level API for interacting with parent threads
--| *from* the worker module.
module Effect.Worker.Child ( sendMsg
                           , onMsg
                           ) where

import Prelude

import Web.Event.Message (MessageEvent)
import Effect (Effect)
import Effect.Uncurried ( EffectFn1
                        , EffectFn2
                        , runEffectFn1
                        , runEffectFn2
                        )
import Effect.Unsafe (unsafePerformEffect)

--| Internal binding for the `postMessage` function
--| available in the global scope of web worker modules.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/DedicatedWorkerGlobalScope/postMessage)
--|
--| ```purescript
--| import Effect.Worker (Worker, sendMsg)
--|
--| worker :: Worker String _
--|
--| sendMsg "hello" worker
--| ```
sendMsg :: ∀ up. up -> Effect Unit
sendMsg = runEffectFn1 sendMsg_

--| Internal binding for attaching a listener to the `onmessage` property
--| of the global scope of web worker modules.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/DedicatedWorkerGlobalScope/onmessage)
--|
--| ```purescript
--| import Effect.Worker (Worker, sendMsg)
--|
--| worker :: Worker String _
--|
--| onMsg (\(m :: String) -> log m) worker
--| ```
onMsg :: ∀ dn. (MessageEvent dn -> Effect Unit) -> Effect Unit
onMsg = runEffectFn2 onMsg_ $ unsafePerformEffect

foreign import sendMsg_ :: ∀ up. EffectFn1 up Unit
foreign import onMsg_ :: ∀ a dn. EffectFn2 (Effect a -> a) (dn -> Effect Unit) Unit
