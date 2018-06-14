module Note where

import Prelude ((-), (+), class Show, class Eq, (==), otherwise)

import Interval (Interval)

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

incNoteBy :: Note -> Interval -> Note
incNoteBy note 0 = note
incNoteBy note n = incNoteBy (incNote note) (n - 1)

decNoteBy :: Note -> Interval -> Note
decNoteBy note 0 = note
decNoteBy note n = decNoteBy (decNote note) (n - 1)

noteDistance :: Note -> Note -> Interval
noteDistance from to
  | from == to = 0
  | otherwise = 1 + (noteDistance (incNote from) to)
