module Scale where

import Data.List (List(..), fromFoldable)
import Data.Set (Set)
import Data.Set as S
import Interval (Interval)
import Interval as I
import Note (Note, incNoteBy)
import Tagged (Tagged(..))

data Scale = Scale (List Interval)

majorScale :: Scale
majorScale = Scale (fromFoldable [ I.majorSecond
                                 , I.majorSecond
                                 , I.minorSecond
                                 , I.majorSecond
                                 , I.majorSecond
                                 , I.majorSecond
                                 , I.minorSecond
                                 ])

dorian :: Scale
dorian = Scale (fromFoldable [ I.majorSecond
                             , I.minorSecond
                             , I.majorSecond
                             , I.majorSecond
                             , I.majorSecond
                             , I.minorSecond
                             , I.majorSecond
                             ])

phrygian :: Scale
phrygian = Scale (fromFoldable [ I.minorSecond
                               , I.majorSecond
                               , I.majorSecond
                               , I.majorSecond
                               , I.minorSecond
                               , I.majorSecond
                               , I.majorSecond
                               ])

mixolydian :: Scale
mixolydian = Scale (fromFoldable [ I.majorSecond
                                 , I.majorSecond
                                 , I.majorSecond
                                 , I.minorSecond
                                 , I.majorSecond
                                 , I.majorSecond
                                 , I.minorSecond
                                 ])

lydian :: Scale
lydian = Scale (fromFoldable [ I.majorSecond
                             , I.majorSecond
                             , I.minorSecond
                             , I.majorSecond
                             , I.majorSecond
                             , I.minorSecond
                             , I.majorSecond
                             ])

minorScale :: Scale
minorScale = Scale (fromFoldable [ I.majorSecond
                                 , I.minorSecond
                                 , I.majorSecond
                                 , I.majorSecond
                                 , I.minorSecond
                                 , I.majorSecond
                                 , I.majorSecond
                                 ])

locrian :: Scale
locrian = Scale (fromFoldable [ I.minorSecond
                              , I.majorSecond
                              , I.majorSecond
                              , I.minorSecond
                              , I.majorSecond
                              , I.majorSecond
                              , I.majorSecond
                              ])

minorPentatonic :: Scale
minorPentatonic = Scale (fromFoldable [ I.minorThird
                                      , I.majorSecond
                                      , I.majorSecond
                                      , I.minorThird
                                      ])

majorPentatonic :: Scale
majorPentatonic = Scale (fromFoldable [ I.majorSecond
                                      , I.majorSecond
                                      , I.minorThird
                                      , I.minorThird
                                      ])

allScales :: Array (Tagged Scale)
allScales = [ Tagged "Major / Ionic" majorScale
            , Tagged "Dorian" dorian
            , Tagged "Phrygian" phrygian
            , Tagged "Mixolydian" mixolydian
            , Tagged "Lydian" lydian
            , Tagged "Minor / Aeolian" minorScale
            , Tagged "Locrian" locrian
            , Tagged "Minor Pentatonic" minorPentatonic
            , Tagged "Major Pentatonic" majorPentatonic
            ]

type ThisScale = { rootNote :: Note
                 , scale :: Set Note
                 }

generateScaleHelper :: Scale -> Note -> List Note
generateScaleHelper (Scale Nil) note = Cons note Nil
generateScaleHelper (Scale (Cons interval rest)) note =
  Cons note (generateScaleHelper (Scale rest) nextNote)
  where nextNote = incNoteBy note interval

generateScale :: Scale -> Note -> ThisScale
generateScale scaleIntervals note =
  { rootNote: note
  , scale: S.fromFoldable (generateScaleHelper scaleIntervals note)
  }
