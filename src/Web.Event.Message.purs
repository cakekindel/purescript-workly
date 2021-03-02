module Web.Event.Message where

import Prelude
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))

--| A `MessageEvent` fired by calls to `Worker#postMessage`
--| or `DedicatedWorkerGlobalScope#postMessage`.
--|
--| [MDN](https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent)
foreign import data MessageEvent :: Type -> Type

messageData :: ∀ a. MessageEvent a -> Maybe a
messageData = (runFn3 data_) Just Nothing

foreign import data_ :: ∀ a. Fn3 (a -> Maybe a) -- Just
                                 (Maybe a)      -- Nothing
                                 (MessageEvent a)
                                 (Maybe a)
