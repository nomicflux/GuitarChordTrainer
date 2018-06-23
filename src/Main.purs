module Main where

import Prelude

import Component.Guitar as G
import Effect (Effect)
import Halogen.Aff (runHalogenAff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- HA.awaitBody
  runUI G.component unit body
