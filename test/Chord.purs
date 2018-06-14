module Test.Chord where

import Prelude
import Data.List (fromFoldable)
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Chord as C

chordTests :: Effect Unit
chordTests = run [consoleReporter] do
  describe "chords" do
    it "major" do
      (C.generateChord C.majorChord C.C) `shouldEqual` (fromFoldable [C.C, C.E, C.G])
    it "minor" do
      (C.generateChord C.minorChord C.C) `shouldEqual` (fromFoldable [C.C, C.DsEb, C.G])
    it "diminished" do
      (C.generateChord C.diminishedChord C.C) `shouldEqual` (fromFoldable [C.C, C.DsEb, C.FsGb])
    it "augmented" do
      (C.generateChord C.augmentedChord C.C) `shouldEqual` (fromFoldable [C.C, C.E, C.GsAb])
    it "dom7thChord" do
      (C.generateChord C.dom7thChord C.C) `shouldEqual` (fromFoldable [C.C, C.E, C.G, C.AsBb])
    it "maj7thChord" do
      (C.generateChord C.major7thChord C.C) `shouldEqual` (fromFoldable [C.C, C.E, C.G, C.B])
    it "min7thChord" do
      (C.generateChord C.minor7thChord C.C) `shouldEqual` (fromFoldable [C.C, C.DsEb, C.G, C.AsBb])
    it "majmin7thChord" do
      (C.generateChord C.majorMinor7thChord C.C) `shouldEqual` (fromFoldable [C.C, C.DsEb, C.G, C.B])
    it "halfdimChord" do
      (C.generateChord C.halfDiminishedChord C.C) `shouldEqual` (fromFoldable [C.C, C.DsEb, C.FsGb, C.AsBb])
    it "dim7thChord" do
      (C.generateChord C.diminished7thChord C.C) `shouldEqual` (fromFoldable [C.C, C.DsEb, C.FsGb, C.A])
    it "aug7thChord" do
      (C.generateChord C.augmented7thChord C.C) `shouldEqual` (fromFoldable [C.C, C.E, C.GsAb, C.AsBb])
