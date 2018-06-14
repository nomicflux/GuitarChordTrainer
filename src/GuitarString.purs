module GuitarString where

import Note (Note, noteDistance)

type GuitarString = { baseNote :: Note }

findFret :: GuitarString -> Note -> Int
findFret string note = noteDistance string.baseNote note
