module GuitarString where

import Interval (Interval, toInt)
import Note (Note, noteDistance, incNoteBy, decNoteBy)

type GuitarString = { baseNote :: Note }

type Fret = { position :: Int
            , note :: Note
            }

toFret :: Interval -> Note -> Fret
toFret interval note = { position: toInt interval
                       , note
                       }

mkString :: Note -> GuitarString
mkString note = { baseNote: note }

findFret :: GuitarString -> Note -> Fret
findFret string note = toFret (noteDistance string.baseNote note) note

transposeString :: (Note -> Interval -> Note) -> Interval -> GuitarString -> GuitarString
transposeString f interval string =
  string { baseNote = f string.baseNote interval }

transposeStringUp :: Interval -> GuitarString -> GuitarString
transposeStringUp = transposeString incNoteBy

transposeStringDown :: Interval -> GuitarString -> GuitarString
transposeStringDown = transposeString decNoteBy
