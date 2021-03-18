--| Module containing Channels for communicating across Worker thread boundaries
--|
--| ```purescript run
--| > -- setup required to run examples
--| > import Effect.Aff.Link
--| > import Effect (Effect)
--| > import Effect.Unsafe (unsafePerformEffect)
--| > import Effect.Aff (Aff)
--| > import Test.Deasync (unsafeBlockOnAff)
--| > runAff = unsafeBlockOnAff
--| > runEff = unsafePerformEffect
--| ```
module Effect.Aff.Link ( Link(..)
                       , send
                       , recv
                       , Channel
                       , snap
                       , put
                       , read
                       , take
                       , newChannel
                       , newChannelHot
                       , newChannelFromCb
                       , ImpureCb
                       , AttachImpureCb
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

--| A 2-way communication link (uplink & downlink)
--| for communicating to & from some separate context (e.g. a Worker thread)
newtype Link up dn = Link {up :: Channel up, down :: Channel dn}

--| Send a message in the uplink channel of a `Link`.
--|
--| ```purescript run
--| > sendExUp :: Channel String
--|   sendExUp = runAff newChannel
--| > sendExDown :: Channel Unit
--|   sendExDown = runAff newChannel
--| > sendEx = Link {up: sendExUp, down: sendExDown}
--| > runAff $ send "here's a message!" sendEx
--| unit
--| > -- now whoever's upstream will get our message
--| ```
send :: ∀ u. u -> Link u _ -> Aff Unit
send u (Link {up}) = put u up

--| Blocks until a message is available in the downlink channel of a `Link`.
--| Once a message is available, clears the downlink channel and yields the message.
--|
--| ```purescript run
--| > recvExUp :: Channel Unit
--|   recvExUp = runAff newChannel
--| > recvExDown :: Channel String
--|   recvExDown = runAff newChannel
--| > recvEx = Link {up: recvExUp, down: recvExDown}
--| > -- emulate a message being sent by someone upstream
--| > runAff $ put "hello!" recvExDown
--| unit
--| > runAff $ recv recvEx
--| "hello!"
--| ```
recv :: ∀ d. Link _ d -> Aff d
recv (Link {down}) = take down

--| A one-way channel storing some mutable state `a`,
--| capable of performing side-effects on update (such as `Worker#postMessage`).
--|
--| Is very similar to the `AVar` structure from `purescript-avar`,
--| but allows attaching change listeners which allows JS-style event listeners
--| to be used as data sources.
--|
--| Can be constructed with `newChannelFromCb`, `newChannel`, `newChannelHot`.
data Channel a = Channel (AVar a)
               | ChannelHot (AVar a) (ImpureCb a)

_getVar :: ∀ a. Channel a -> AVar a
_getVar = case _ of
            Channel var      -> var
            ChannelHot var _ -> var

--| Create a new empty one-way channel, with empty initial state.
--|
--| Calls to `read` or `take` will block until a value is inserted.
--|
--| ```purescript run
--| > numChannel :: Channel Int
--|   numChannel = runAff newChannel
--| > runAff $ put 12 numChannel
--| unit
--| > runAff $ read numChannel
--| 12
--| ```
newChannel :: ∀ a. Aff (Channel a)
newChannel = do
           v <- Var.empty
           pure $ Channel v

type ImpureCb a = a -> Effect Unit
type AttachImpureCb a = ImpureCb a -> Effect Unit

--| Create a one-way channel that sources its data from an asynchronous (JS) callback.
--|
--| ```purescript run
--| > import Effect.Ref (Ref)
--| > import Effect.Ref as R
--| > -- create a mutable ref to mimic JS code
--|   -- attaching a callback to an async source
--| > cbRef :: Ref (String -> Effect Unit)
--|   cbRef = runEff $ R.new \_ -> mempty
--| > setCb = \cb -> R.modify_ (const cb) cbRef
--| > -- create a channel that gives our "JS" code
--|   -- a callback to invoke
--| > cbEx = runAff $ newChannelFromCb setCb
--| > -- invoke the callback
--| > cb = runEff $ R.read cbRef
--| > runEff $ cb "hello"
--| unit
--| > runAff $ snap cbEx
--| (Just "hello")
--| ```
newChannelFromCb :: ∀ a. AttachImpureCb a -> Aff (Channel a)
newChannelFromCb attach = do
                            uni <- newChannel
                            let cb = \a -> do
                                             _ <- launchAff $ put a uni
                                             mempty
                            liftEffect $ attach cb
                            pure uni

--| Create a one-way channel that invokes some callback when
--| a message is put into the channel.
--|
--| ```purescript run
--| > import Effect.Ref as Ref
--| > -- set up a piece of mutable state to demo performing
--| > -- side-effects on `put`
--| > didPut = runEff $ Ref.new false
--| > setDidPut :: String -> Effect Unit
--|   setDidPut _ = Ref.modify_ (const true) didPut
--| > onPutEx :: Channel String
--|   onPutEx = runAff $ newChannelHot setDidPut
--| > runAff $ put "testing" onPutEx
--| unit
--| > runEff $ Ref.read didPut
--| true
--| ```
newChannelHot :: ∀ a. ImpureCb a -> Aff (Channel a)
newChannelHot f = do
                    var <- Var.empty
                    pure $ ChannelHot var f

--| Wait for a value to be inserted into a channel,
--| then yield it without modifying the state.
--|
--| ```purescript run
--| > readEx :: Channel Int
--|   readEx = runAff newChannel
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
read :: ∀ a. Channel a -> Aff a
read = _getVar >>> Var.read

--| Wait for a value to be inserted into the `Channel`,
--| then yield it and empty the `Channel`.
--|
--| ```purescript run
--| > takeEx :: Channel Int
--|   takeEx = runAff newChannel
--| > runAff $ put 999 takeEx
--| unit
--| > runAff $ snap takeEx
--| (Just 999)
--| > runAff $ take takeEx
--| 999
--| > runAff $ snap takeEx
--| Nothing
--| ```
take :: ∀ a. Channel a -> Aff a
take = _getVar >>> Var.take

--| Get a snapshot of the value in a `Channel`directional channel.
--|
--| Similar to `read`, but returns `Nothing` if there is no
--| value in the channel, rather than blocking until a value exists.
--|
--| ```purescript run
--| > snapEx :: Channel String
--|   snapEx = runAff newChannel
--| > runAff $ put "testing" snapEx
--| unit
--| > runAff $ snap snapEx
--| (Just "testing")
--| > runAff $ take snapEx
--| "testing"
--| > runAff $ snap snapEx
--| Nothing
--| ```
snap :: ∀ a. Channel a -> Aff (Maybe a)
snap = _getVar >>> Var.tryRead

--| Discard any value currently in the `Channel` and insert
--| a new one.
--|
--| This will notify all listeners of an update, triggering
--| any necessary side-effects (such as `Worker#postMessage`)
--|
--| Wait for a value to be inserted into the `Channel`,
--| then yield it and empty the `Channel`.
--|
--| ```purescript run
--| > putEx :: Channel String
--|   putEx = runAff $ newChannel
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
put :: ∀ a. a -> Channel a -> Aff Unit
put a chan = do
               _ <- Var.tryTake v    -- flush the avar
               _ <- Var.tryPut a v   -- put new value
               liftEffect $ cbEffect -- notify listener
  where
    v = _getVar chan
    cbEffect = case chan of
                 ChannelHot _ f -> f a
                 _              -> mempty
