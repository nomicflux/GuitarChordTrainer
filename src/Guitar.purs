module Guitar where

import Prelude ((<$>), (-))
import Data.Array (length, (!!))
import Data.Maybe (Maybe)

import GuitarString (GuitarString, mkString)
import Note (Note)
import Note as N

type Guitar = { strings :: Array GuitarString }

mkGuitar :: Array Note -> Guitar
mkGuitar notes = { strings: mkString <$> notes }

numStrings :: Guitar -> Int
numStrings guitar = length guitar.strings

getString :: Guitar -> Int -> Maybe GuitarString
getString guitar n = guitar.strings !! (numStrings guitar - n)

standardGuitar :: Guitar
standardGuitar = mkGuitar [N.E, N.A, N.D, N.G, N.B, N.E]

dropD :: Guitar
dropD = mkGuitar [N.D, N.A, N.D, N.G, N.B, N.D]

standard8String :: Guitar
standard8String = mkGuitar [N.FsGb, N.B, N.E, N.A, N.D, N.G, N.B, N.E]

mandolin :: Guitar
mandolin = mkGuitar [N.G, N.D, N.A, N.E]
