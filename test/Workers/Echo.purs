module Test.Workers.Echo where

import Prelude

import Web.Event.Message (messageData)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Worker (Worker, Module)
import Effect.Worker.Child (onMsg, sendMsg)
import Test.Logging (log)

type EchoWorker = Worker String String

main :: Effect Unit
main = onMsg handle
  where
    getMsg = messageData >>> maybe "" identity
    handle msgEv = do
                     let msg = getMsg msgEv
                     log "Test.Workers.Echo"
                         ("received " <> show msg)
                     sendMsg msg
