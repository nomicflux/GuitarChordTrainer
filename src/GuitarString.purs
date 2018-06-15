module GuitarString where

import Interval (toInt)
import Note (Note, noteDistance)

type GuitarString = { baseNote :: Note }

type Fret = Int

toFret :: Interval -> Fret
toFret = identity . toInt

mkString :: Note -> GuitarString
mkString note = { baseNote: note }

findFret :: GuitarString -> Note -> Fret
findFret string note = toFret (noteDistance string.baseNote note)
