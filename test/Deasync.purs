module Test.Deasync where

import Prelude
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Aff (Aff)
import Control.Promise as Promise

foreign import deasync :: ∀ a. (Effect (Promise.Promise a)) -> Effect a

unsafeBlockOnAff :: ∀ a. Aff a -> a
unsafeBlockOnAff = Promise.fromAff >>> deasync >>> unsafePerformEffect
