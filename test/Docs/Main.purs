module Test.Docs.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import DocTest as DocTest

main :: Effect Unit
main = launchAff_
     $ runSpec [consoleReporter]
     $ DocTest.main
