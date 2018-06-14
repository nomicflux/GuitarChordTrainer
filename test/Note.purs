module Test.Note where

import Prelude
import Data.List (fromFoldable)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Interval as I
import Note as N

noteSpec :: Spec Unit
noteSpec =
  describe "note distance" do
    it "same note" do
      (N.noteDistance N.A N.A) `shouldEqual` I.sameNote
    it "half step" do
      (N.noteDistance N.A N.AsBb) `shouldEqual` I.minorSecond
    it "fifth" do
      (N.noteDistance N.A N.E) `shouldEqual` I.fifth
    it "major seventh" do
      (N.noteDistance N.C N.B) `shouldEqual` I.majorSeventh
