module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.WaiSpec as WaiSpec

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] WaiSpec.spec
