module Component.Constants where

import Prelude ((*), (+), (/))

pushedFretRadius :: Int
pushedFretRadius = 5

fretMarkerRadius :: Int
fretMarkerRadius = 6

fretWidth :: Int
fretWidth = 30

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
