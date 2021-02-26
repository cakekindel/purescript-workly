module Effect.Worker.Parent where

import Prelude

import Effect (Effect)
import Effect.Worker

--| binding for `new Worker`
foreign import spawn :: ∀ req res. String -> Effect (Worker req res)

--| binding for `Worker#sendMessage`
foreign import sendMsg :: ∀ req. Worker req _ -> req -> Effect Unit

--| binding for attaching a listener to `Worker#onmessage`
foreign import onMsg :: ∀ res
                      . Worker _ res
                     -> (res -> Effect Unit)
                     -> Effect Unit
