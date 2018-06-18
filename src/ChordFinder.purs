module ChordFinder where

import Control.Monad.Reader (Reader)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S

import Guitar (Guitar)
import GuitarString (Fret)
import Chord (ThisChord)
import Note (Note)

type Window = { above :: Int
              , below :: Int
              }

type PotentialChord = Array (Maybe Fret)

type ValidationStep = PotentialChord -> Reader ChordContext Boolean

type ChordContext = { guitar :: Guitar
                    , window :: Window
                    , startingString :: Maybe Int
                    , baseNotes :: Set Note
                    }

mkSymWindow :: Int -> Window
mkSymWindow n = { above: n, below: n }

mkContext :: Guitar -> Int -> ChordContext
mkContext guitar n = { guitar
                     , window: mkSymWindow n
                     , startingString: Nothing
                     , baseNotes: S.empty
                     }

withStartingString :: ChordContext -> Int -> ChordContext
withStartingString ctx n = ctx { startingString = Just n }

withBaseNote :: ChordContext -> Note -> ChordContext
withBaseNote ctx note = ctx { baseNotes = S.fromFoldable [note] }

withChord :: ChordContext -> ThisChord -> ChordContext
withChord ctx chord = ctx { baseNotes = chord.chord }

withRoot :: ChordContext -> ThisChord -> ChordContext
withRoot ctx chord = ctx { baseNotes = S.fromFoldable [chord.rootNote] }

getAllChords :: Guitar -> Chord -> Set ThisChord
