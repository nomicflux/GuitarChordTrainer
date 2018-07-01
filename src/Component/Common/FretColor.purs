module Component.Common.FretColor where

import Color as Color
import Data.Int (toNumber)
import Prelude (mod, (*), (/), (==))

fretColor :: Int -> String
fretColor interval =
  let hue = (toNumber interval) / 12.0 * 360.0
      saturation = if interval `mod` 2 == 0 then 1.0 else 0.6
      lightness = if interval `mod` 2 == 0 then 0.5 else 0.7
      alpha = 1.0
      color = Color.hsla hue saturation lightness alpha
  in
   Color.toHexString color
