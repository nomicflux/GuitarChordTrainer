module Test.Main where

import Prelude
import Effect (Effect)

import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Test.Chord (chordTests)
import Test.Note (noteTests)

main :: Effect Unit
main = run [consoleReporter] do
  noteTests
  chordTests
