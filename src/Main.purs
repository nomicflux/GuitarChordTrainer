module Main where

import Prelude

import Component.Guitar as CG
import Effect (Effect)
import Guitar as G
import Halogen.Aff (runHalogenAff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- HA.awaitBody
  runUI CG.component G.standardGuitar body
