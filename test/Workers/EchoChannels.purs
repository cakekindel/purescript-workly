module Test.Workers.EchoChannels where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Worker (Worker)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Worker (parentChannels)
import Effect.Aff.Worker.Channel as Channel
import Control.Monad.Rec.Class (forever)
import Test.Logging (log)

type EchoChannelsWorker = Worker String String

main :: Effect Unit
main = launchAff_ $ do
         let log' = log "Test.Workers.EchoChannels" >>> liftEffect
         link <- parentChannels :: Aff (Channel.Bi String String)
         log' "created link to parent comms"

         _ <- forever do
                        msg <- Channel.recv link
                        log' $ "got " <> show msg
                        Channel.send msg link
                        log' "sent it back"
         mempty :: Aff Unit
