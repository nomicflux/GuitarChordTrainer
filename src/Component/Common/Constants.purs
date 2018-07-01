module Component.Common.Constants where

import Halogen as H
import Prelude ((*), (+), (/))

pushedFretRadius :: Int
pushedFretRadius = 8

fretMarkerRadius :: Int
fretMarkerRadius = 10

fretWidth :: Int
fretWidth = 30

fretHeight :: Int
fretHeight = 3 * fretWidth / 2

halfFretWidth :: Int
halfFretWidth = fretWidth / 2

halfFretHeight :: Int
halfFretHeight = fretHeight / 2

lineHeight :: Int -> Int
lineHeight fret = (fret + 1) * fretHeight

stringLength :: Int -> Int
stringLength frets = lineHeight frets

bodyRef :: H.RefLabel
bodyRef = H.RefLabel "MainBody"

guitarCookie :: String
guitarCookie = "guitar"

scaleChordCookie :: String
scaleChordCookie = "scale-or-chord"
