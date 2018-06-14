module Test.Note where

import Prelude
import Data.List (fromFoldable)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Note as N

noteSpec :: Spec Unit
noteSpec =
  describe "note distance" do
    it "same note" do
      (N.noteDistance N.A N.A) `shouldEqual` 0
    it "half step" do
      (N.noteDistance N.A N.AsBb) `shouldEqual` 1
    it "fifth" do
      (N.noteDistance N.A N.E) `shouldEqual` 7
    it "major seventh" do
      (N.noteDistance N.C N.B) `shouldEqual` 11
