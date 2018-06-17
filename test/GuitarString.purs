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
      (GS.findFret (GS.mkString N.C) N.C) `shouldEqual` {position: 0, note: N.C}
    it "fret minor third" do
      (GS.findFret (GS.mkString N.E) N.G) `shouldEqual` {position: 3, note: N.G}
    it "fret fifth" do
      (GS.findFret (GS.mkString N.A) N.E) `shouldEqual` {position: 7, note: N.E}
