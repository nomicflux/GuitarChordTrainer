module Test.Main where

import Prelude
import Effect (Effect)

import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Test.Chord (chordSpec)
import Test.Guitar (guitarSpec)
import Test.GuitarString (guitarStringSpec)
import Test.Note (noteSpec)

main :: Effect Unit
main = run [consoleReporter] do
  noteSpec
  chordSpec
  guitarStringSpec
  guitarSpec
