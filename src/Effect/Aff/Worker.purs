module Effect.Aff.Worker ( parentChannels
                         , workerChannels
                         ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Worker (Worker, onMsg, sendMsg)
import Effect.Worker.Child as Child
import Effect.Aff (Aff, launchAff)
import Effect.Aff.Worker.Channel as Channel
import Web.Event.Message (MessageEvent, messageData)
import Data.Maybe (Maybe(..))

--| Construct bi-directional channels for
--| a worker to be able to communicate with the parent thread.
--|
--| ```purs
--| module GreetWorker where
--|
--| import Prelude
--|
--| import Effect.Worker (Worker)
--| import Effect.Aff (liftAff)
--| import Effect.Aff.Worker (parentChannels)
--| import Effect.Aff.Worker.Channel (Bi, parentChannels)
--| import Control.Monad.Rec.Class (forever)
--|
--| data GreetWorker = Worker String String
--|
--| main :: Effect Unit
--| main = liftAff $ forever do
--|   link <- parentChannels :: Aff (Bi String String)
--|   name <- recv link -- wait for parent to send us a name
--|   let msg = "Hello, " <> name <> "!"
--|   _ <- send msg link -- send "hello, name!" to parent context
--| ```
parentChannels :: ∀ dn up. Aff (Channel.Bi dn up)
parentChannels =
  do
    down <- Channel.uniFromCb (onMsg' $ Child.onMsg)
    up   <-  Channel.newUni
    let up' = Channel.onPut (Child.sendMsg) up
    pure $ Channel.Bi {down, up: up'}

--| Construct bi-directional channels for
--| a worker to be able to communicate with the parent thread.
--|
--| ```purescript
--| module Main where
--|
--| import Prelude
--|
--| import Effect.Console (log)
--| import Effect.Aff (liftAff)
--| import Effect.Aff.Worker (parentChannels)
--| import Effect.Aff.Worker.Channel (Bi, parentChannels)
--| import Control.Monad.Rec.Class (forever)
--| import GreetWorker (GreetWorker)
--|
--| foreign import spawnGreetWorker :: Effect GreetWorker
--|
--| main :: Effect Unit
--| main = liftAff $ do
--|   worker <- spawnGreetWorker
--|   link <- workerChannels worker
--|   _ <- send "Harry" link
--|   greeting <- recv link -- wait for greeter to send us a greeting
--|   _ <- liftEffect $ log greeting
--| ```
workerChannels :: ∀ dn up. Worker dn up -> Aff (Channel.Bi dn up)
workerChannels worker =
  do
    down <- Channel.uniFromCb (onMsg' $ onMsg worker)
    up   <- Channel.newUni
    let up' = Channel.onPut (sendMsg worker) up
    pure $ Channel.Bi {down, up: up'}

type MsgCb a = MessageEvent a -> Effect Unit
type AttachMsgCb a = MsgCb a -> Effect Unit

--| Given a function that attaches a listener for a `MessageEvent`, an effectful callback to attach, attaches the callback.
--| The callback will be invoked when `MessageEvent`s are fired with a `data` property set.
onMsg' :: ∀ a
        . AttachMsgCb a
       -> (a -> Effect Unit)
       -> Effect Unit
onMsg' attach cb = attach
                     \ev -> case messageData ev of
                       Just msg -> cb msg
                       Nothing  -> mempty
