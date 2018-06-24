module Interval where

import Prelude (identity, otherwise, (==))

type Interval = Int

-- To keep a stable API
toInt :: Interval -> Int
toInt = identity

sameNote :: Interval
sameNote = 0

minorSecond :: Interval
minorSecond = 1

majorSecond :: Interval
majorSecond = 2

minorThird :: Interval
minorThird = 3

majorThird :: Interval
majorThird = 4

fourth :: Interval
fourth = 5

augmentedFourth :: Interval
augmentedFourth = 6

diminishedFifth :: Interval
diminishedFifth = 6

fifth :: Interval
fifth = 7

minorSixth :: Interval
minorSixth = 8

majorSixth :: Interval
majorSixth = 9

minorSeventh :: Interval
minorSeventh = 10

majorSeventh :: Interval
majorSeventh = 11

octave :: Interval
octave = sameNote

minorNinth :: Interval
minorNinth = minorSecond

majorNinth :: Interval
majorNinth = majorSecond

eleventh :: Interval
eleventh = fourth

minorThirteenth :: Interval
minorThirteenth = minorSixth

majorThirteenth :: Interval
majorThirteenth = majorSixth

intervalToName :: Interval -> String
intervalToName interval
  | interval == sameNote = "Root"
  | interval == minorThird = "Minor Third"
  | interval == majorThird = "Major Third"
  | interval == diminishedFifth = "Tritone"
  | interval == fifth = "Fifth"
  | interval == minorSeventh = "Minor Seventh"
  | interval == majorSeventh = "Major Seventh"
  | interval == minorNinth = "Minor Second / Ninth"
  | interval == majorNinth = "Major Second / Ninth"
  | interval == eleventh = "Fourth / Eleventh"
  | interval == minorThirteenth = "Minor Sixth / Thirteenth"
  | interval == majorThirteenth = "Major Sixth / Thirteenth"
  | otherwise = "_"
