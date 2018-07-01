module Main where

import Prelude

import Component.App as App
import Component.Common.Constants (guitarCookie)
import Component.Common.Cookie (getCookie)
import Effect (Effect)
import Halogen as H
import Halogen.Aff (runHalogenAff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- HA.awaitBody
  cookie <- H.liftEffect $ getCookie guitarCookie
  runUI App.component cookie body
