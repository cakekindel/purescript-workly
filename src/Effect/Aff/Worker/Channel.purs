module Effect.Aff.Worker.Channel ( Uni
                                 , Bi(..)
                                 , send
                                 , recv
                                 , put
                                 , read
                                 , take
                                 , newUni
                                 , uniFromCb
                                 , onPut
                                 ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff)
import Effect.AVar (AVar)
import Effect.Aff.AVar as Var
import Control.Monad.State (State)
import Data.Array ((:))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

--| A bi-directional channel (uplink & downlink)
--| for communicating to | from a worker thread.
newtype Bi up dn = Bi {up :: Uni up, down :: Uni dn}

--| Send a message up a `Bi`directional channel.
send :: ∀ u. u -> Bi u _ -> Aff Unit
send u (Bi {up}) = put u up

--| Wait for a message to be sent down the `Bi`directional channel,
--| and take it from the downlink `Uni` channel.
recv :: ∀ d. Bi _ d -> Aff d
recv (Bi {down}) = take down

--| A `Uni`directional channel storing some mutable state `a`,
--| capable of performing side-effects on update (such as `Worker#postMessage`).
--|
--| Is very similar to the `AVar` structure from `purescript-avar`,
--| but allows attaching change listeners which allows JS-style event listeners
--| to be used as data sources.
data Uni a = Uni (AVar a) (Array (ChangeCb a))

--| Create a new empty one-way channel, with empty initial state.
--|
--| Calls to `read` or `take` will block until a value is inserted.
newUni :: ∀ a. Aff (Uni a)
newUni = do
           v <- Var.empty
           pure $ Uni v []

type Cb a = a -> Effect Unit
type AttachCb a = Cb a -> Effect Unit

--| Create a `Uni`directional channel from an effectful function
--| that attaches a JS-style callback listener to some data source.
--|
--| That data source will then be piped into the new `Uni`directional
--| channel, `put`ting data into the channel on invocation.
uniFromCb :: ∀ a. AttachCb a -> Aff (Uni a)
uniFromCb attach = do
                     uni <- newUni
                     let cb = \a -> do
                                      _ <- launchAff $ put a uni
                                      mempty
                     liftEffect $ attach cb
                     pure uni

--| Wait for a value to be inserted into a channel,
--| then yield it without modifying the state.
read :: ∀ a. Uni a -> Aff a
read (Uni v _) = Var.read v

--| Wait for a value to be inserted into the `Uni`,
--| then yield it and empty the `Uni`.
take :: ∀ a. Uni a -> Aff a
take (Uni v _) = Var.take v

--| Discard any value currently in the `Uni` and insert
--| a new one.
--|
--| This will notify all listeners of an update, triggering
--| any necessary side-effects (such as `Worker#postMessage`)
put :: ∀ a. a -> Uni a -> Aff Unit
put a chan@(Uni v cbs) = do
                             _ <- Var.tryTake v
                             _ <- Var.tryPut a v
                             liftEffect $ invokeCbs chan a
                             mempty

--| A `Uni#put` listener
type ChangeCb a = a -> Effect Unit

--| Attach a change listener (`a -> Effect Unit`) to a
--| `Uni`directional channel.
--|
--| Note that this function does _not_ mutate
--| the `Uni` and returns a new `Uni` containing a reference
--| to the same mutable state with the new listener attached.
onPut :: ∀ a. ChangeCb a -> Uni a -> Uni a
onPut f (Uni v fs) = Uni v (f : fs)

--| Invoke all `ChangeCb`s attached to a `Uni` of an update.
--|
--| Note that this will invoke the callbacks in the opposite order
--| that they were attached, since `Data.Array.cons` is used to attach new listeners.
invokeCbs :: ∀ a. Uni a -> a -> Effect Unit
invokeCbs (Uni _ fs) a = for_ fs \f -> f a
