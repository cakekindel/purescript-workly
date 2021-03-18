module Test.Workers.EchoChannels where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Worker (Worker)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Worker.Link (parentLink)
import Effect.Aff.Link as Link
import Control.Monad.Rec.Class (forever)
import Test.Logging (log)

type EchoChannelsWorker = Worker String String

main :: Effect Unit
main = launchAff_ $ do
         let log' = log "Test.Workers.EchoChannels" >>> liftEffect
         link <- parentLink :: Aff (Link.Link String String)
         log' "created link to parent comms"

         _ <- forever do
                        msg <- Link.recv link
                        log' $ "got " <> show msg
                        Link.send msg link
                        log' "sent it back"
         mempty :: Aff Unit
