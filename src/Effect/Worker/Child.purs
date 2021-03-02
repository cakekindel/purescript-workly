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
import Effect.Worker

--| Internal binding for the `postMessage` function
--| available in the global scope of web worker modules.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/DedicatedWorkerGlobalScope/postMessage)
sendMsg :: ∀ req. req -> Effect Unit
sendMsg = runEffectFn1 sendMsg_

--| Internal binding for attaching a listener to the `onmessage` property
--| of the global scope of web worker modules.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/DedicatedWorkerGlobalScope/onmessage)
onMsg :: ∀ res. (MessageEvent res -> Effect Unit) -> Effect Unit
onMsg = runEffectFn2 onMsg_ $ unsafePerformEffect

foreign import sendMsg_ :: ∀ req. EffectFn1 req Unit

foreign import onMsg_ :: ∀ res. EffectFn2 (Effect Unit -> Unit)
                                          (res -> Effect Unit)
                                          Unit
