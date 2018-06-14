module GuitarString where

import Interval (toInt)
import Note (Note, noteDistance)

type GuitarString = { baseNote :: Note }

mkString :: Note -> GuitarString
mkString note = { baseNote: note }

findFret :: GuitarString -> Note -> Int
findFret string note = toInt (noteDistance string.baseNote note)
