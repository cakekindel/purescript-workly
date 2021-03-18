module Effect.Aff.Worker.Link ( parentLink
                              , workerLink
                              ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Worker (Worker, onMsg, sendMsg)
import Effect.Worker.Child as Child
import Effect.Aff (Aff, launchAff)
import Effect.Aff.Link as Link
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
--| import Effect.Aff.Worker.Link as Link
--| import Control.Monad.Rec.Class (forever)
--|
--| data GreetWorker = Worker String String
--|
--| main :: Effect Unit
--| main = liftAff $ forever do
--|   link <- parentLink :: Aff (Link.Link String String)
--|
--|   -- wait for parent to send us a name
--|   name <- Link.recv link
--|
--|   -- send "hello, name!" to parent thread
--|   let msg = "Hello, " <> name <> "!"
--|   _ <- Link.send msg link
--|
--|   mempty
--| ```
parentLink :: ∀ dn up. Aff (Link.Link dn up)
parentLink =
  do
    down <- Link.newChannelFromCb (onMsg' $ Child.onMsg)
    up   <-  Link.newChannelHot (Child.sendMsg)
    pure $ Link.Link {down, up}

--| Construct a communication link for a Worker,
--| allowing 2-way communication with a worker thread.
--|
--| ```purescript
--| module Main where
--|
--| import Prelude
--|
--| import Effect.Console (log)
--| import Effect.Aff (liftAff)
--| import Effect.Aff.Worker.Link (workerLink)
--| import Effect.Aff.Link as Link
--| import Control.Monad.Rec.Class (forever)
--| import GreetWorker (GreetWorker)
--|
--| foreign import spawnGreetWorker :: Effect GreetWorker
--|
--| main :: Effect Unit
--| main = liftAff $ do
--|   worker <- spawnGreetWorker
--|   link   <- workerLink worker
--|
--|   Link.send "Harry" link
--|   greeting <- Link.recv link
--|
--|   liftEffect $ log greeting
--|   mempty
--| ```
workerLink :: ∀ dn up. Worker dn up -> Aff (Link.Link dn up)
workerLink worker =
  do
    down <- Link.newChannelFromCb (onMsg' $ onMsg worker)
    up   <- Link.newChannelHot (sendMsg worker)
    pure $ Link.Link {down, up}

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
