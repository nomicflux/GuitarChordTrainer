module ChordFinder where

import Control.Monad.Reader (Reader)
import Data.List (List)
import Data.Maybe (Maybe)

import Guitar (Guitar)
import Guitar as G
import GuitarString (GuitarString, Fret)
import Chord (Chord)
import Chord as C
import Note (N)

type PotentialChord = Array (Maybe Fret)

type ValidationStep = PotentialChord -> Reader ChordContext Boolean

type ChordContext = { guitar :: Guitar
                    , window :: Int
                    , startingString :: Maybe Int
                    , baseNote :: Maybe Note
                    }
