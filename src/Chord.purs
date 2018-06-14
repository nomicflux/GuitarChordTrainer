module Chord where

import Prelude
import Data.List (List(..), fromFoldable)

import Note (Note(..))
import Tagged (Tagged(..))

data Chord = Chord (List Int)

majorChord :: Chord
majorChord = Chord (fromFoldable [4, 3])

minorChord :: Chord
minorChord = Chord (fromFoldable [3, 4])

diminishedChord :: Chord
diminishedChord = Chord (fromFoldable [3, 3])

augmentedChord :: Chord
augmentedChord = Chord (fromFoldable [4, 4])

dom7thChord :: Chord
dom7thChord = Chord (fromFoldable [4, 3, 3])

major7thChord :: Chord
major7thChord = Chord (fromFoldable [4, 3, 4])

minor7thChord :: Chord
minor7thChord = Chord (fromFoldable [3, 4, 3])

majorMinor7thChord :: Chord
majorMinor7thChord = Chord (fromFoldable [3, 4, 4])

halfDiminishedChord :: Chord
halfDiminishedChord = Chord (fromFoldable [3, 3, 4])

diminished7thChord :: Chord
diminished7thChord = Chord (fromFoldable [3, 3, 3])

augmented7thChord :: Chord
augmented7thChord = Chord (fromFoldable [4, 4, 2])

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
