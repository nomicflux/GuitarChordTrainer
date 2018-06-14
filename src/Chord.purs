module Chord where

import Prelude
import Data.List (List(..), fromFoldable)

data Note = A | AsBb | B | C | CsDb | D | DsEb | E | F | FsGb | G | GsAb

derive instance eqNote :: Eq Note

instance showNote :: Show Note where
  show A = "A"
  show AsBb = "A#/Bb"
  show B = "B"
  show C = "C"
  show CsDb = "C#/Db"
  show D = "D"
  show DsEb = "D#/Eb"
  show E = "E"
  show F = "F"
  show FsGb = "F#/Gb"
  show G = "G"
  show GsAb = "G#/Ab"

incNote :: Note -> Note
incNote A = AsBb
incNote AsBb = B
incNote B = C
incNote C = CsDb
incNote CsDb = D
incNote D = DsEb
incNote DsEb = E
incNote E = F
incNote F = FsGb
incNote FsGb = G
incNote G = GsAb
incNote GsAb = A

decNote :: Note -> Note
decNote A = GsAb
decNote AsBb = A
decNote B = AsBb
decNote C = B
decNote CsDb = C
decNote D = CsDb
decNote DsEb = D
decNote E = DsEb
decNote F = E
decNote FsGb = F
decNote G = FsGb
decNote GsAb = G

incNoteBy :: Note -> Int -> Note
incNoteBy note 0 = note
incNoteBy note n = incNoteBy (incNote note) (n - 1)

decNoteBy :: Note -> Int -> Note
decNoteBy note 0 = note
decNoteBy note n = decNoteBy (decNote note) (n - 1)

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

data Tagged a = Tagged String a

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
