--| Module containing Channels for communicating across Worker thread boundaries
--|
--| ```purescript run
--| > -- setup required to run examples
--| > import Effect.Aff.Worker.Channel
--| > import Effect (Effect)
--| > import Effect.Unsafe (unsafePerformEffect)
--| > import Effect.Aff (Aff)
--| > import Test.Deasync (unsafeBlockOnAff)
--| > runAff = unsafeBlockOnAff
--| > runEff = unsafePerformEffect
--| ```
module Effect.Aff.Worker.Channel ( Uni
                                 , Bi(..)
                                 , send
                                 , snap
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
--|
--| ```purescript run
--| > sendExUp :: Uni String
--|   sendExUp = runAff newUni
--| > sendExDown :: Uni Unit
--|   sendExDown = runAff newUni
--| > sendEx = Bi {up: sendExUp, down: sendExDown}
--| > runAff $ send "here's a message!" sendEx
--| unit
--| > -- now whoever's upstream will get our message
--| ```
send :: ∀ u. u -> Bi u _ -> Aff Unit
send u (Bi {up}) = put u up

--| Wait for a message to be sent down the `Bi`directional channel,
--| and take it from its downlink `Uni` channel.
--|
--| ```purescript run
--| > recvExUp :: Uni Unit
--|   recvExUp = runAff newUni
--| > recvExDown :: Uni String
--|   recvExDown = runAff newUni
--| > recvEx = Bi {up: recvExUp, down: recvExDown}
--| > -- emulate a message being sent by someone upstream
--| > runAff $ put "hello!" recvExDown
--| unit
--| > runAff $ recv recvEx
--| "hello!"
--| ```
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
--|
--| ```purescript run
--| > numChannel :: Uni Int
--|   numChannel = runAff newUni
--| > runAff $ put 12 numChannel
--| unit
--| > runAff $ read numChannel
--| 12
--| ```
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
--| ```purescript run
--| > readEx :: Uni Int
--|   readEx = runAff newUni
--|   -- danger!
--|   --   reading now will block forever since nobody will
--|   --   `put` a value in the channel!
--| > runAff $ put 123 readEx
--| unit
--| > runAff $ read readEx
--| 123
--| > runAff $ read readEx
--| 123
--| ```
read :: ∀ a. Uni a -> Aff a
read (Uni v _) = Var.read v

--| Wait for a value to be inserted into the `Uni`,
--| then yield it and empty the `Uni`.
--|
--| ```purescript run
--| > takeEx :: Uni Int
--|   takeEx = runAff newUni
--| > runAff $ put 999 takeEx
--| unit
--| > runAff $ snap takeEx
--| (Just 999)
--| > runAff $ take takeEx
--| 999
--| > runAff $ snap takeEx
--| Nothing
--| ```
take :: ∀ a. Uni a -> Aff a
take (Uni v _) = Var.take v

--| Get a snapshot of the value in a `Uni`directional channel.
--|
--| Similar to `read`, but returns `Nothing` if there is no
--| value in the channel, rather than blocking until a value exists.
--|
--| ```purescript run
--| > snapEx :: Uni String
--|   snapEx = runAff newUni
--| > runAff $ put "testing" snapEx
--| unit
--| > runAff $ snap snapEx
--| (Just "testing")
--| > runAff $ take snapEx
--| "testing"
--| > runAff $ snap snapEx
--| Nothing
--| ```
snap :: ∀ a. Uni a -> Aff (Maybe a)
snap (Uni v _) = Var.tryRead v

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
--| > putEx :: Uni String
--|   putEx = runAff $ newUni
--| > runAff $ snap putEx
--| Nothing
--| > runAff $ put "testing" putEx
--| unit
--| > runAff $ snap putEx
--| (Just "testing")
--| > runAff $ put "testing2" putEx
--| unit
--| > runAff $ read putEx
--| "testing2"
--| ```
put :: ∀ a. a -> Uni a -> Aff Unit
put a chan@(Uni v cbs) = do
                           _ <- Var.tryTake v            -- flush the avar
                           _ <- Var.tryPut a v           -- put new value
                           liftEffect $ invokeCbs chan a -- notify listeners
                           mempty                        -- done

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
--| > import Effect.Ref as Ref
--| > onPutEx :: Uni String
--|   onPutEx = runAff newUni
--| > -- set up a piece of mutable state to demo performing
--| > -- side-effects on `put`
--| > didPut = runEff $ Ref.new false
--| > updateDone :: ∀ a. a -> Effect Unit
--|   updateDone _ = Ref.modify_ (const true) didPut
--| > -- attach listener to `onPutEx`
--| > onPutEx' = onPut updateDone onPutEx
--| > runAff $ put "testing" onPutEx'
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
