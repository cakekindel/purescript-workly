module Effect.Worker.Child where

import Prelude

import Effect (Effect, unsafePerformEffect)
import Effect.Worker

--| Internal binding for `global.sendMessage`
foreign import send :: ∀ req. req -> Effect Unit

--| Internal binding for attaching a listener to `global.onmessage`
listen :: ∀ res. (res -> Effect Unit) -> Effect Unit
listen = listen_ unsafePerformEffect

foreign import listen_ :: ∀ res
                        . (Effect Unit -> Unit)
                       -> (res -> Effect Unit)
                       -> Effect Unit
