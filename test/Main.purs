module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Test.Chord (chordTests)

main :: Effect Unit
main = do
  chordTests
