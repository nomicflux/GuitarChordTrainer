module Guitar where

import Prelude

import GuitarString (GuitarString, mkString)
import Note (Note)

type Guitar = { strings :: Array GuitarString }

mkGuitar :: Array Note -> Guitar
mkGuitar notes = { strings: mkString <$> notes }
