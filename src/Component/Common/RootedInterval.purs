module Component.Common.RootedInterval where

import Chord (ThisChord)
import Control.Category ((>>>))
import Data.Eq (class Eq)
import Data.Function (identity)
import Data.Ord (class Ord, compare)
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (uncurry)
import Interval (Interval)
import Note (Note)
import Note as N
import Scale (ThisScale)

type RootedInterval = { rootNote :: Note
                      , notes :: Set Note
                      }

fromScale :: ThisScale -> RootedInterval
fromScale scale = { rootNote: scale.rootNote
                  , notes: scale.scale
                  }

fromChord :: ThisChord -> RootedInterval
fromChord chord = { rootNote: chord.rootNote
                  , notes: chord.chord
                  }

filterNotes :: RootedInterval -> Set Note -> RootedInterval
filterNotes intervals notes = intervals { notes = S.difference intervals.notes notes }

data IntervalledNote = IntervalledNote Interval Note

getInterval :: IntervalledNote -> Interval
getInterval (IntervalledNote i _) = i

getNote :: IntervalledNote -> Note
getNote (IntervalledNote _ n) = n

mkIntervalledNote :: Interval -> Note -> IntervalledNote
mkIntervalledNote interval note = IntervalledNote interval note

derive instance eqIntervalledNote :: Eq IntervalledNote
instance ordIntervalledNote :: Ord IntervalledNote where
  compare a b = compare (getInterval a) (getInterval b)

toIntervals :: RootedInterval -> Set IntervalledNote
toIntervals intervals =
  S.map ((N.noteDistance intervals.rootNote &&& identity) >>> uncurry mkIntervalledNote) intervals.notes
