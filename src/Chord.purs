module Chord where

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

allChords :: List (Tagged Chord)
allChords = fromFoldable [ Tagged "Major" majorChord
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

generateChord :: Chord -> Note -> List Note
generateChord (Chord Nil) note = Cons note Nil
generateChord (Chord (Cons interval rest)) note =
  Cons note (generateChord (Chord rest) nextNote)
  where nextNote = incNoteBy note interval
