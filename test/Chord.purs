module Test.Chord where

import Prelude
import Data.List (fromFoldable)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Chord as C
import Note as N

chordTests :: Spec Unit
chordTests =
  describe "chords" do
    it "major" do
      (C.generateChord C.majorChord N.C) `shouldEqual` (fromFoldable [N.C, N.E, N.G])
    it "minor" do
      (C.generateChord C.minorChord N.C) `shouldEqual` (fromFoldable [N.C, N.DsEb, N.G])
    it "diminished" do
      (C.generateChord C.diminishedChord N.C) `shouldEqual` (fromFoldable [N.C, N.DsEb, N.FsGb])
    it "augmented" do
      (C.generateChord C.augmentedChord N.C) `shouldEqual` (fromFoldable [N.C, N.E, N.GsAb])
    it "dom7thChord" do
      (C.generateChord C.dom7thChord N.C) `shouldEqual` (fromFoldable [N.C, N.E, N.G, N.AsBb])
    it "maj7thChord" do
      (C.generateChord C.major7thChord N.C) `shouldEqual` (fromFoldable [N.C, N.E, N.G, N.B])
    it "min7thChord" do
      (C.generateChord C.minor7thChord N.C) `shouldEqual` (fromFoldable [N.C, N.DsEb, N.G, N.AsBb])
    it "majmin7thChord" do
      (C.generateChord C.majorMinor7thChord N.C) `shouldEqual` (fromFoldable [N.C, N.DsEb, N.G, N.B])
    it "halfdimChord" do
      (C.generateChord C.halfDiminishedChord N.C) `shouldEqual` (fromFoldable [N.C, N.DsEb, N.FsGb, N.AsBb])
    it "dim7thChord" do
      (C.generateChord C.diminished7thChord N.C) `shouldEqual` (fromFoldable [N.C, N.DsEb, N.FsGb, N.A])
    it "aug7thChord" do
      (C.generateChord C.augmented7thChord N.C) `shouldEqual` (fromFoldable [N.C, N.E, N.GsAb, N.AsBb])
