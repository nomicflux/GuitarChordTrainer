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

minorScale :: Scale
minorScale = Scale (fromFoldable [ I.majorSecond
                                 , I.minorSecond
                                 , I.majorSecond
                                 , I.majorSecond
                                 , I.minorSecond
                                 , I.majorSecond
                                 , I.majorSecond
                                 ])

allScales :: Array (Tagged Scale)
allScales = [ Tagged "Major / Ionic" majorScale
            , Tagged "Minor / Aeolian" minorScale
            ]

type ThisScale = { rootNote :: Note, scale :: Set Note }

generateScaleHelper :: Scale -> Note -> List Note
generateScaleHelper (Scale Nil) note = Cons note Nil
generateScaleHelper (Scale (Cons interval rest)) note =
  Cons note (generateScaleHelper (Scale rest) note)
  where nextNote = incNoteBy note interval

generateScale :: Scale -> Note -> ThisScale
generateScale scaleIntervals note = { rootNote: note
                                    , scale: S.fromFoldable (generateScaleHelper scaleIntervals note)}
