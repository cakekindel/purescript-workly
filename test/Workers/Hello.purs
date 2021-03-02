module Test.Workers.Hello where

import Prelude
import Effect (Effect)
import Effect.Worker (Worker)
import Test.Logging (log)

type HelloWorker = Worker Unit Unit

main :: Effect Unit
main = log "Test.Workers.Hello" "Hello, world!"
