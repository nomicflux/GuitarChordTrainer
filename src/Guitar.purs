module Guitar where

import Data.Array (length, (!!))
import Data.Maybe (Maybe)
import GuitarString (GuitarString, mkString, transposeStringDown, transposeStringUp)
import Interval (Interval)
import Note (Note)
import Note as N
import Prelude ((<$>), (-))
import Tagged (Tagged(..))

type Guitar = { strings :: Array GuitarString }

mkGuitar :: Array Note -> Guitar
mkGuitar notes = { strings: mkString <$> notes }

numStrings :: Guitar -> Int
numStrings guitar = length guitar.strings

getString :: Guitar -> Int -> Maybe GuitarString
getString guitar n = guitar.strings !! (numStrings guitar - n)

standardGuitar :: Guitar
standardGuitar = mkGuitar [N.E, N.A, N.D, N.G, N.B, N.E]

dropD :: Guitar
dropD = mkGuitar [N.D, N.A, N.D, N.G, N.B, N.E]

openG :: Guitar
openG = mkGuitar [N.D, N.G, N.D, N.G, N.B, N.D]

standard7String :: Guitar
standard7String = mkGuitar [N.B, N.E, N.A, N.D, N.G, N.B, N.E]

standard8String :: Guitar
standard8String = mkGuitar [N.FsGb, N.B, N.E, N.A, N.D, N.G, N.B, N.E]

dropE8String :: Guitar
dropE8String = mkGuitar [N.E, N.B, N.E, N.A, N.D, N.G, N.B, N.E]

ukulele :: Guitar
ukulele = mkGuitar [N.G, N.C, N.E, N.A]

mandolin :: Guitar
mandolin = mkGuitar [N.G, N.D, N.A, N.E]

standard5StringBanjo :: Guitar
standard5StringBanjo = mkGuitar [N.G, N.D, N.G, N.B, N.D]

bass :: Guitar
bass = mkGuitar [N.E, N.A, N.D, N.G]

fiveStringBass :: Guitar
fiveStringBass = mkGuitar [N.B, N.E, N.A, N.D, N.G]

allFourths :: Guitar
allFourths = mkGuitar [N.E, N.A, N.D, N.G, N.C, N.F]

majorThirds :: Guitar
majorThirds = mkGuitar [N.GsAb, N.C, N.E, N.GsAb, N.C, N.E]

majorThirds7String :: Guitar
majorThirds7String = mkGuitar [N.E, N.GsAb, N.C, N.E, N.GsAb, N.C, N.E]

majorThirds8String :: Guitar
majorThirds8String = mkGuitar [N.C, N.E, N.GsAb, N.C, N.E, N.GsAb, N.C, N.E]

allGuitars :: Array (Tagged Guitar)
allGuitars = [ Tagged "Standard Tuning" standardGuitar
             , Tagged "Drop D Tuning" dropD
             , Tagged "Open G Tuning" openG
             , Tagged "All Fourths" allFourths
             , Tagged "7 String Guitar" standard7String
             , Tagged "8 String Guitar" standard8String
             , Tagged "Drop E 8 String" dropE8String
             , Tagged "Mandolin" mandolin
             , Tagged "Ukulele" ukulele
             , Tagged "Banjo" standard5StringBanjo
             , Tagged "Bass" bass
             , Tagged "5 String Bass" fiveStringBass
             , Tagged "Major 3rds" majorThirds
             , Tagged "Major 3rds 7 String" majorThirds7String
             , Tagged "Major 3rds 8 String" majorThirds8String
             ]

transposeGuitarUp :: Interval -> Guitar -> Guitar
transposeGuitarUp interval guitar =
  guitar { strings = (transposeStringUp interval) <$> guitar.strings }

transposeGuitarDown :: Interval -> Guitar -> Guitar
transposeGuitarDown interval guitar =
  guitar { strings = (transposeStringDown interval) <$> guitar.strings }
