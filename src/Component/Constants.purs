module Component.Constants where

import Prelude ((*), (+), (/))

fretWidth :: Int
fretWidth = 20

halfFretWidth :: Int
halfFretWidth = fretWidth / 2

halfStringWidth :: Int
halfStringWidth = 10

fretHeight :: Int -> Int
fretHeight fret = (fret + 1) * fretWidth

stringLength :: Int -> Int
stringLength frets = fretHeight frets
