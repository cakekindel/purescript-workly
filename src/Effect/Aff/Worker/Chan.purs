module Effect.Aff.Worker.Chan ( Chan
                              , Chans
                              , put
                              , read
                              , take
                              , newChan
                              , onChange
                              ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect.AVar (AVar)
import Effect.Aff.AVar as Var
import Control.Monad.State (State)
import Data.Array ((:))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

type Chans up dn = {down :: Chan dn, up :: Chan up}

data Chan a = Chan (AVar a) (Array (PutListener a))

type PutListener a = a -> Effect Unit

onChange :: ∀ a. PutListener a -> Chan a -> Chan a
onChange f (Chan v fs) = Chan v (f : fs)

newChan :: ∀ a. Aff (Chan a)
newChan = do
            v <- Var.empty
            pure $ Chan v []

read :: ∀ a. Chan a -> Aff a
read (Chan v _) = Var.read v

take :: ∀ a. Chan a -> Aff a
take (Chan v _) = Var.take v

put :: ∀ a. a -> Chan a -> Aff Unit
put a (Chan v listeners) = do
                             _ <- Var.tryTake v
                             _ <- Var.tryPut a v
                             liftEffect $ notifyListeners
                             mempty
  where
    notifyListeners :: Effect Unit
    notifyListeners = for_ listeners \f -> f a

