module Component.Constants where

import Halogen as H
import Prelude ((*), (+), (/))

pushedFretRadius :: Int
pushedFretRadius = 7

fretMarkerRadius :: Int
fretMarkerRadius = 9

fretWidth :: Int
fretWidth = 40

fretHeight :: Int
fretHeight = fretWidth

halfFretWidth :: Int
halfFretWidth = fretWidth / 2

halfFretHeight :: Int
halfFretHeight = halfFretWidth

lineHeight :: Int -> Int
lineHeight fret = (fret + 1) * fretWidth

stringLength :: Int -> Int
stringLength frets = lineHeight frets

bodyRef :: H.RefLabel
bodyRef = H.RefLabel "MainBody"
