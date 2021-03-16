--| Module containing Channels for communicating across Worker thread boundaries
--|
--| ```purescript run
--| > -- setup code for doc comments
--| > import Effect.Unsafe (unsafePerformEffect)
--| > import Effect.Aff (Aff, makeAff)
--| > import Effect.Aff.Worker.Channel (Uni, newUni, put, read, onPut)
--| > import Test.Deasync (unsafeBlockOnAff)
--| > runAff = unsafeBlockOnAff
--| > runEff = unsafePerformEffect
--| > strChannel = runAff (newUni :: Aff (Uni String))
--| ```
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
                                 , Cb
                                 , AttachCb
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

--| A `Bi`directional channel (uplink & downlink)
--| for communicating to & from some other context (e.g. a Worker thread)
newtype Bi up dn = Bi {up :: Uni up, down :: Uni dn}

--| Send a message up a `Bi`directional channel.
send :: ∀ u. u -> Bi u _ -> Aff Unit
send u (Bi {up}) = put u up

--| Wait for a message to be sent down the `Bi`directional channel,
--| and take it from its downlink `Uni` channel.
--|
--| ```purs
--| module Main where
--|
--| import Prelude
--|
--| import Effect (Effect)
--| import Effect.Class (liftEffect)
--| import Effect.Aff (launchAff)
--| import Effect.Worker (Worker)
--| import Effect.Aff.Worker (workerChannels)
--|
--| -- fictional worker that echoes a message back twice
--| -- without modifying
--| type EchoWorker = Worker String String
--| foreign import spawnEcho :: Effect EchoWorker
--|
--| main :: Effect Unit
--| main = launchAff $ do
--|          echo <- liftEffect $ spawnEcho
--|          link <- workerChannels echo
--|
--|          send "hello" link
--|          msg1 <- recv link
--|          -- second `recv` call will wait
--|          -- for a second message to be sent.
--|          msg2 <- recv link
--|
--|          mempty
--| ```
recv :: ∀ d. Bi _ d -> Aff d
recv (Bi {down}) = take down

--| A `Uni`directional channel storing some mutable state `a`,
--| capable of performing side-effects on update (such as `Worker#postMessage`).
--|
--| Is very similar to the `AVar` structure from `purescript-avar`,
--| but allows attaching change listeners which allows JS-style event listeners
--| to be used as data sources.
--|
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
--|
--| ```purs
--| module Main where
--|
--| import Prelude
--|
--| import Effect (Effect)
--| import Effect.Aff (launchAff_)
--| import Effect.Aff.Worker.Channel (Uni, newUni)
--|
--| main :: Effect Unit
--| main = launchAff_
--|      $ do
--|          channel <- newUni :: Aff (Uni String)
--|
--|          put "testing" channel
--|          val   <- read channel -- "testing"
--|          val'  <- read channel -- "testing"
--|
--|          put "testing2"
--|          val'' <- read channel -- "testing2"
--|
--|          mempty
--| ```
read :: ∀ a. Uni a -> Aff a
read (Uni v _) = Var.read v

--| Wait for a value to be inserted into the `Uni`,
--| then yield it and empty the `Uni`.
--|
--| ```purescript run
--| > runAff $ put "testing" strChannel
--| unit
--| > runAff $ read strChannel
--| "testing"
--| > runAff $ read strChannel
--| "testing"
--| > runAff $ put "testing2" strChannel
--| unit
--| > runAff $ read strChannel
--| "testing2"
--| ```
take :: ∀ a. Uni a -> Aff a
take (Uni v _) = Var.take v

--| Discard any value currently in the `Uni` and insert
--| a new one.
--|
--| This will notify all listeners of an update, triggering
--| any necessary side-effects (such as `Worker#postMessage`)
--|
--| Wait for a value to be inserted into the `Uni`,
--| then yield it and empty the `Uni`.
--|
--| ```purescript run
--| > runAff $ put "testing" strChannel
--| unit
--| > runAff $ read strChannel
--| "testing"
--| > runAff $ put "testing2" strChannel
--| unit
--| > runAff $ read strChannel
--| "testing2"
--| ```
put :: ∀ a. a -> Uni a -> Aff Unit
put a chan@(Uni v cbs) = do
                           _ <- Var.tryTake v
                           _ <- Var.tryPut a v
                           liftEffect $ invokeCbs chan a
                           mempty

--| A side-effecting callback
type ChangeCb a = a -> Effect Unit

--| Attach a change listener (`a -> Effect Unit`) to a
--| `Uni`directional channel.
--|
--| Note that this function does _not_ mutate
--| the `Uni` and returns a new `Uni` containing a reference
--| to the same mutable state with the new listener attached.
--|
--| ```purescript run
--| > -- set up a piece of mutable state to demo performing
--| > -- side-effects on `put`
--| > import Effect.Ref as Ref
--| > didPut = runEff $ Ref.new false
--| > updateDone _ = Ref.modify_ (const true) didPut
--| > -- attach listener to `strChannel`
--| > hotChannel = onPut updateDone strChannel
--| > runAff $ put "testing" hotChannel
--| unit
--| > runEff $ Ref.read didPut
--| true
--| ```
onPut :: ∀ a. ChangeCb a -> Uni a -> Uni a
onPut f (Uni v fs) = Uni v (f : fs)

--| Invoke all `ChangeCb`s attached to a `Uni` of an update.
--|
--| Note that this will invoke the callbacks in the opposite order
--| that they were attached, since `Data.Array.cons` is used to attach new listeners.
invokeCbs :: ∀ a. Uni a -> a -> Effect Unit
invokeCbs (Uni _ fs) a = for_ fs \f -> f a
