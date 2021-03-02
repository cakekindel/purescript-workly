module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, makeAff, forkAff, joinFiber)
import Effect.Aff.Worker (workerChans)
import Effect.Aff.Worker.Chan as Chan
import Data.Foldable (for_)
import Data.Either (Either(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Mocha (runMocha)

import Test.Logging (log)
import Test.Workers.Hello (HelloWorker)
import Test.Workers.Echo (EchoWorker)

import Web.Event.Message (messageData)
import Effect.Worker as Worker

foreign import spawnWorker :: { hello :: Effect HelloWorker
                              , echo  :: Effect EchoWorker
                              }

foreign import isWorker :: ∀ a. a -> Boolean

main :: Effect Unit
main =
  let
    toAff :: ∀ a. Effect a -> Aff a
    toAff = liftEffect
    msgAff worker = makeAff \cb -> do
                         Worker.onMsg worker (Right >>> cb)
                         mempty
  in
    runMocha do
      describe "Worker" do
        it "should create a new Worker" do
          toAff $ log "(hello) Test.Main" "created empty worker"
          worker <- toAff $ spawnWorker.hello
          worker `shouldSatisfy` isWorker

        it "should be able to use `sendMsg` and `onMsg` for 2-way messaging" do
          let log' = log "(echo_low_level) Test.Main" >>> toAff

          -- ARRANGE
          worker <- toAff $ spawnWorker.echo
          log' "created echo worker"

          msgFork <- forkAff $ msgAff worker
          log' "ready for message..."

          -- ACT
          let expected = "hello!"
          toAff $ Worker.sendMsg worker expected
          log' $ "sent " <> show expected

          -- ASSERT
          msgEv <- joinFiber msgFork
          let msg = messageData msgEv
          log' $ "received " <> show msg
          msg `shouldEqual` (pure expected)

        it "should be able to use Channels for 2-way messaging" do
          let log' = log "(echo_chans) Test.Main" >>> toAff

          -- ARRANGE
          worker <- toAff $ spawnWorker.echo
          log' "created echo worker"

          {up, down} <- workerChans worker

          -- ACT
          let cases = ["hello!", "hello2!", "wow!"]
          for_ cases \expected -> do
            Chan.put expected up
            log' $ "sent " <> show expected

            -- ASSERT
            msg <- Chan.take down
            log' $ "received " <> show msg
            msg `shouldEqual` expected
