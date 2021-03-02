module Effect.Aff.Worker where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff)
import Effect.Worker (Worker, onMsg, sendMsg)
import Effect.Aff.Worker.Chan (Chan, Chans, put, newChan, onChange)
import Web.Event.Message (MessageEvent, messageData)
import Data.Maybe (Maybe(..))

type MessageEventListener d = MessageEvent d -> Effect Unit
type AttachMessageEventListener d = MessageEventListener d -> Effect Unit

workerChans :: forall dn up. Worker dn up -> Aff (Chans dn up)
workerChans worker =
  do
    down <- mkDownlinkChan (onMsg worker)
    up   <- newChan
    let up' = onChange (sendMsg worker) up
    pure $ {down, up: up'}

mkDownlinkChan :: forall dn
                . AttachMessageEventListener dn
               -> Aff (Chan dn)
mkDownlinkChan onDn = do
                        chan <- newChan
                        liftEffect $
                          onDn \ev -> case messageData ev of
                                 Just msg -> do
                                    _ <- launchAff $ put msg chan
                                    mempty
                                 _        -> mempty
                        pure chan

