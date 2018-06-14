module String where

import Note (Note, noteDistance)

type String = String { baseNote :: Note }

findFret :: String -> Note -> Int
findFret string note = noteDistance (baseNote string) note
