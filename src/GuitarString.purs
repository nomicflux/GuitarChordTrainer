module GuitarString where

import Note (Note, noteDistance)

type GuitarString = { baseNote :: Note }

mkString :: Note -> GuitarString
mkString note = { baseNote: note }

findFret :: GuitarString -> Note -> Int
findFret string note = noteDistance string.baseNote note
