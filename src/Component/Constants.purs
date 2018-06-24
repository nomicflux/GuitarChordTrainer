module Component.Constants where

import Prelude ((*), (+), (/))

pushedFretRadius :: Int
pushedFretRadius = 5

fretMarkerRadius :: Int
fretMarkerRadius = 6

fretWidth :: Int
fretWidth = 30

halfFretWidth :: Int
halfFretWidth = fretWidth / 2

halfStringWidth :: Int
halfStringWidth = halfFretWidth

fretHeight :: Int -> Int
fretHeight fret = (fret + 1) * fretWidth

stringLength :: Int -> Int
stringLength frets = fretHeight frets
