module Effect.Worker.Parent ( spawn
                            , sendMsg
                            , onMsg
                            ) where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried ( EffectFn1
                        , EffectFn2
                        , EffectFn3
                        , runEffectFn1
                        , runEffectFn2
                        , runEffectFn3
                        )
import Effect.Worker

--| Create a worker thread from a relpath to the corresponding JS module
--|
--| TODO: usage examples (i don't even know how to use this yet)
spawn :: ∀ req res. ModulePath -> Effect (Worker req res)
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
onMsg :: ∀ res. Worker _ res -> (res -> Effect Unit) -> Effect Unit
onMsg = runEffectFn3 onMsg_ $ unsafePerformEffect

foreign import spawn_   :: ∀ req res. EffectFn1 ModulePath (Worker req res)
foreign import sendMsg_ :: ∀ req. EffectFn2 (Worker req _) req Unit
foreign import onMsg_   :: ∀ a res. EffectFn3 (Effect a -> a) (Worker _ res) (res -> Effect Unit) Unit
