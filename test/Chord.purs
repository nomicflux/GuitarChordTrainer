module Test.Chord where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Set (fromFoldable)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Chord as C
import Interval as I
import Note as N

chordSpec :: Spec Unit
chordSpec = do
  describe "chords" do
    it "major" do
      C.generateChord C.majorChord N.C `shouldEqual` { rootNote: N.C, chord: fromFoldable [N.C, N.E, N.G] }
    it "minor" do
      C.generateChord C.minorChord N.C `shouldEqual` { rootNote: N.C, chord: fromFoldable [N.C, N.DsEb, N.G] }
    it "diminished" do
      C.generateChord C.diminishedChord N.C `shouldEqual` { rootNote: N.C, chord: fromFoldable [N.C, N.DsEb, N.FsGb] }
    it "augmented" do
      C.generateChord C.augmentedChord N.C `shouldEqual` { rootNote: N.C, chord: fromFoldable [N.C, N.E, N.GsAb] }
    it "dom7thChord" do
      C.generateChord C.dom7thChord N.C `shouldEqual` { rootNote: N.C, chord: fromFoldable [N.C, N.E, N.G, N.AsBb] }
    it "maj7thChord" do
      C.generateChord C.major7thChord N.C `shouldEqual` { rootNote: N.C, chord: fromFoldable [N.C, N.E, N.G, N.B] }
    it "min7thChord" do
      C.generateChord C.minor7thChord N.C `shouldEqual` { rootNote: N.C, chord: fromFoldable [N.C, N.DsEb, N.G, N.AsBb] }
    it "majmin7thChord" do
      C.generateChord C.majorMinor7thChord N.C `shouldEqual` { rootNote: N.C, chord: fromFoldable [N.C, N.DsEb, N.G, N.B] }
    it "halfdimChord" do
      C.generateChord C.halfDiminishedChord N.C `shouldEqual` { rootNote: N.C, chord: fromFoldable [N.C, N.DsEb, N.FsGb, N.AsBb] }
    it "dim7thChord" do
      C.generateChord C.diminished7thChord N.C `shouldEqual` { rootNote: N.C, chord: fromFoldable [N.C, N.DsEb, N.FsGb, N.A] }
    it "aug7thChord" do
      C.generateChord C.augmented7thChord N.C `shouldEqual` { rootNote: N.C, chord: fromFoldable [N.C, N.E, N.GsAb, N.AsBb] }
  describe "transpose" do
    it "a major third" do
      C.transposeChord (C.generateChord C.majorChord N.C) I.majorThird `shouldEqual` { rootNote: N.E, chord: fromFoldable [N.E, N.GsAb, N.B] }
  describe "inversions" do
    it "get major third" do
      C.getInversionBase (C.generateChord C.majorChord N.C) I.majorThird `shouldEqual` Just N.E
    it "minor seventh" do
      C.getInversionBase (C.generateChord C.dom7thChord N.C) I.minorSeventh `shouldEqual` Just N.AsBb
    it "nothing when absent" do
      C.getInversionBase (C.generateChord C.majorChord N.C) I.fourth `shouldEqual` Nothing
  describe "additions / removals" do
    it "adds flat ninth" do
      C.addInterval (C.generateChord C.dom7thChord N.C) I.minorNinth `shouldEqual` { rootNote: N.C
                                                                                   , chord: fromFoldable [N.C, N.E, N.G, N.AsBb, N.CsDb]
                                                                                   }
    it "removes fifth" do
      C.removeInterval (C.generateChord C.dom7thChord N.C) I.fifth `shouldEqual` { rootNote: N.C
                                                                                 , chord: fromFoldable [N.C, N.E, N.AsBb]
                                                                                 }
