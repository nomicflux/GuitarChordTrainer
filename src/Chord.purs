module Chord where

import Data.Array as A
import Data.List (List(..), fromFoldable)

import Interval (Interval)
import Interval as I
import Note (Note, incNoteBy)
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

type ThisChord = { rootNote :: Note, chord :: Array Note }

generateChordHelper :: Chord -> Note -> List Note
generateChordHelper (Chord Nil) note = Cons note Nil
generateChordHelper (Chord (Cons interval rest)) note =
  Cons note (generateChordHelper (Chord rest) nextNote)
  where nextNote = incNoteBy note interval

generateChord :: Chord -> Note -> ThisChord
generateChord chordIntervals note = { rootNote: note
                                    , chord: A.fromFoldable (generateChordHelper chordIntervals note) }
