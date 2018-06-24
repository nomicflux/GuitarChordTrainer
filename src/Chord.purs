module Chord where

import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (uncurry)
import Interval (Interval)
import Interval as I
import Note (Note, incNoteBy)
import Note as N
import Prelude (class Eq, class Ord, compare, identity, (>>>))
import Tagged (Tagged(..))

data Chord = Chord (List Interval)

majorChord :: Chord
majorChord = Chord (fromFoldable [I.majorThird, I.minorThird])

minorChord :: Chord
minorChord = Chord (fromFoldable [I.minorThird, I.majorThird])

diminishedChord :: Chord
diminishedChord = Chord (fromFoldable [I.minorThird, I.minorThird])

augmentedChord :: Chord
augmentedChord = Chord (fromFoldable [I.majorThird, I.majorThird])

dom7thChord :: Chord
dom7thChord = Chord (fromFoldable [I.majorThird, I.minorThird, I.minorThird])

major7thChord :: Chord
major7thChord = Chord (fromFoldable [I.majorThird, I.minorThird, I.majorThird])

minor7thChord :: Chord
minor7thChord = Chord (fromFoldable [I.minorThird, I.majorThird, I.minorThird])

majorMinor7thChord :: Chord
majorMinor7thChord = Chord (fromFoldable [I.minorThird, I.majorThird, I.majorThird])

halfDiminishedChord :: Chord
halfDiminishedChord = Chord (fromFoldable [I.minorThird, I.minorThird, I.majorThird])

diminished7thChord :: Chord
diminished7thChord = Chord (fromFoldable [I.minorThird, I.minorThird, I.minorThird])

augmented7thChord :: Chord
augmented7thChord = Chord (fromFoldable [I.majorThird, I.majorThird, I.majorSecond])

allChords :: Array (Tagged Chord)
allChords = [ Tagged "Major" majorChord
            , Tagged "Minor" minorChord
            , Tagged "Diminished" diminishedChord
            , Tagged "Augmented" augmentedChord
            , Tagged "Dom 7th" dom7thChord
            , Tagged "Maj 7th" major7thChord
            , Tagged "Min 7th" minor7thChord
            , Tagged "Maj Min 7th" majorMinor7thChord
            , Tagged "Half Diminished" halfDiminishedChord
            , Tagged "Diminished 7th" diminished7thChord
            , Tagged "Augmented 7th" augmented7thChord
            ]

type ThisChord = { rootNote :: Note, chord :: Set Note }

generateChordHelper :: Chord -> Note -> List Note
generateChordHelper (Chord Nil) note = Cons note Nil
generateChordHelper (Chord (Cons interval rest)) note =
  Cons note (generateChordHelper (Chord rest) nextNote)
  where nextNote = incNoteBy note interval

generateChord :: Chord -> Note -> ThisChord
generateChord chordIntervals note = { rootNote: note
                                    , chord: S.fromFoldable (generateChordHelper chordIntervals note) }

transposeChord :: ThisChord -> Interval -> ThisChord
transposeChord chord interval = { rootNote: incNoteBy chord.rootNote interval
                                , chord: S.map (\n -> incNoteBy n interval) chord.chord
                                }

retrieve :: forall a. Ord a => a -> Set a -> Maybe a
retrieve a set = if S.member a set then Just a else Nothing

getInversionBase :: ThisChord -> Interval -> Maybe Note
getInversionBase chord interval = retrieve (incNoteBy chord.rootNote interval) chord.chord

addInterval :: ThisChord -> Interval -> ThisChord
addInterval chord interval = chord { chord = S.insert (incNoteBy chord.rootNote interval) chord.chord }

addNote :: ThisChord -> Note -> ThisChord
addNote chord note = chord { chord = S.insert note chord.chord }

removeInterval :: ThisChord -> Interval -> ThisChord
removeInterval chord interval = chord { chord = S.delete (incNoteBy chord.rootNote interval) chord.chord }

removeNote :: ThisChord -> Note -> ThisChord
removeNote chord note = chord { chord = S.delete note chord.chord }

filterNotes :: ThisChord -> Set Note -> ThisChord
filterNotes chord notes = chord { chord = S.difference chord.chord notes }

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

chordToIntervals :: ThisChord -> Set IntervalledNote
chordToIntervals chord =
  S.map ((N.noteDistance chord.rootNote &&& identity) >>> uncurry mkIntervalledNote) chord.chord
