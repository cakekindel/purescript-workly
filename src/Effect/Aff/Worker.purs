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

parentChannels :: forall dn up. Aff (Channel.Bi dn up)
parentChannels =
  do
    down <- Channel.uniFromCb (onMsg' $ Child.onMsg)
    up   <-  Channel.newUni
    let up' = Channel.onPut (Child.sendMsg) up
    pure $ Channel.Bi {down, up: up'}

workerChannels :: forall dn up. Worker dn up -> Aff (Channel.Bi dn up)
workerChannels worker =
  do
    down <- Channel.uniFromCb (onMsg' $ onMsg worker)
    up   <- Channel.newUni
    let up' = Channel.onPut (sendMsg worker) up
    pure $ Channel.Bi {down, up: up'}

type MsgCb a = MessageEvent a -> Effect Unit
type AttachMsgCb a = MsgCb a -> Effect Unit

--| Listen for a worker's  `Nothing`
onMsg' :: ∀ a
        . AttachMsgCb a
       -> (a -> Effect Unit)
       -> Effect Unit
onMsg' attach cb = attach
                     \ev -> case messageData ev of
                       Just msg -> cb msg
                       Nothing  -> mempty
