module Test.GuitarString where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import GuitarString as GS
import Note as N

guitarStringSpec :: Spec Unit
guitarStringSpec =
  describe "guitar string" do
    it "fret base" do
      (GS.findFret (GS.mkString N.C) N.C) `shouldEqual` 0
    it "fret minor third" do
      (GS.findFret (GS.mkString N.E) N.G) `shouldEqual` 3
    it "fret fifth" do
      (GS.findFret (GS.mkString N.A) N.E) `shouldEqual` 7
