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

ukulele :: Guitar
ukulele = mkGuitar [N.G, N.C, N.E, N.A]

mandolin :: Guitar
mandolin = mkGuitar [N.G, N.D, N.A, N.E]

standard5StringBanjo :: Guitar
standard5StringBanjo = mkGuitar [N.G, N.D, N.G, N.B, N.D]

allGuitars :: Array (Tagged Guitar)
allGuitars = [ Tagged "Standard Tuning" standardGuitar
             , Tagged "Drop D Tuning" dropD
             , Tagged "Open G Tuning" openG
             , Tagged "Standard 7 String" standard7String
             , Tagged "Standard 8 String" standard8String
             , Tagged "Mandolin" mandolin
             , Tagged "Ukulele" ukulele
             , Tagged "Standard 5 String Banjo" standard5StringBanjo
             ]

transposeGuitarUp :: Interval -> Guitar -> Guitar
transposeGuitarUp interval guitar =
  guitar { strings = (transposeStringUp interval) <$> guitar.strings }

transposeGuitarDown :: Interval -> Guitar -> Guitar
transposeGuitarDown interval guitar =
  guitar { strings = (transposeStringDown interval) <$> guitar.strings }
