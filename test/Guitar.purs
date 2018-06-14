module Test.Guitar where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Guitar as G
import GuitarString (mkString)
import Note as N

guitarSpec :: Spec Unit
guitarSpec =
  describe "guitar" do
    it "makes standard guitar" do
      let notes = [N.E, N.A, N.D, N.G, N.B, N.E]
      G.mkGuitar notes `shouldEqual` { strings: mkString <$> notes }
    it "makes 8-string guitar" do
      let notes = [N.FsGb, N.B, N.E, N.A, N.D, N.G, N.B, N.E]
      G.mkGuitar notes `shouldEqual` { strings: mkString <$> notes }
    it "makes mandolin" do
      let notes = [N.G, N.D, N.A, N.E]
      G.mkGuitar notes `shouldEqual` { strings: mkString <$> notes }
