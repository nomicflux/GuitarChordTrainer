module Component.GuitarString where

import Prelude

import Chord (ThisChord)
import Color as Color
import Component.Constants (fretHeight, fretWidth, halfFretWidth, halfStringWidth, pushedFretRadius, stringLength)
import Component.FretColor (fretColor)
import Component.SVG as SVG
import Data.Array ((:))
import Data.Array as A
import Data.Int (toNumber)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as S
import GuitarString (Fret, GuitarString, findFret)
import Halogen as H
import Halogen.HTML as HH
import Note (Note)
import Note as N

data FrettedNote = FrettedNote { fret :: Int
                               , fromRoot :: Maybe Int
                               }
derive instance eqFrettedNote :: Eq FrettedNote
instance ordFrettedNote :: Ord FrettedNote where
  compare (FrettedNote a) (FrettedNote b) = a.fret `compare` b.fret

type Input = { string :: GuitarString
             , numFrets :: Int
             , x :: Int
             , onLeft :: Boolean
             , onRight :: Boolean
             }

type State = { string :: GuitarString
             , frets :: Set FrettedNote
             , numFrets :: Int
             , x :: Int
             , onLeft :: Boolean
             , onRight :: Boolean
             , showColor :: Boolean
             }

initialState :: Input -> State
initialState input = { string: input.string
                     , frets: S.empty
                     , numFrets: input.numFrets
                     , x: input.x
                     , onLeft: input.onLeft
                     , onRight: input.onRight
                     , showColor: true
                     }

data Query a = PushFret Fret (Unit -> a)
             | ReleaseFret Fret (Unit -> a)
             | PushChord ThisChord (Unit -> a)
             | ReleaseChord ThisChord (Unit -> a)
             | ShowColor Boolean (Unit -> a)
             | Reset (Unit -> a)

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
  { initialState: initialState
  , render
  , eval
  , receiver: const Nothing
  }

fretColorLess :: Int -> String
fretColorLess interval =
  if interval == 0 then "red" else "black"

renderPushedFret :: forall p i. Boolean -> Int -> FrettedNote -> HH.HTML p i
renderPushedFret showColor x (FrettedNote fret) =
  let f = if showColor then fretColor else fretColorLess
  in
   SVG.circle [ SVG.cx $ halfStringWidth + x
              , SVG.cy $ fretHeight fret.fret - halfFretWidth
              , SVG.r pushedFretRadius
              , SVG.stroke "black"
              , SVG.fill $ maybe "black" f fret.fromRoot
              , SVG.class_ "pushed-fret"
              ]

renderFret :: forall p i. Int -> Boolean -> Boolean -> Int -> Array (HH.HTML p i)
renderFret x onLeft onRight fret =
  let left = if onLeft then [renderLeftFret x fret] else []
      right = if onRight then [renderRightFret x fret] else []
  in left <> right

renderLeftFret :: forall p i. Int -> Int -> HH.HTML p i
renderLeftFret x fret = SVG.line [ SVG.x1 $ x
                                 , SVG.x2 $ halfStringWidth + x
                                 , SVG.y1 $ fretHeight fret
                                 , SVG.y2 $ fretHeight fret
                                 , SVG.stroke "black"
                                 , SVG.class_ "fret"
                                 ]

renderRightFret :: forall p i. Int -> Int -> HH.HTML p i
renderRightFret x fret = SVG.line [ SVG.x1 $ halfStringWidth + x
                                  , SVG.x2 $ halfStringWidth * 2 + x
                                  , SVG.y1 $ fretHeight fret
                                  , SVG.y2 $ fretHeight fret
                                  , SVG.stroke "black"
                                  , SVG.class_ "fret"
                                  ]

render :: State -> H.ComponentHTML Query
render state =
   SVG.svg [] (renderString state.frets)
   where
     renderString :: forall p i. Set FrettedNote -> Array (HH.HTML p i)
     renderString frets =
       SVG.line [ SVG.x1 $ halfStringWidth + state.x
                , SVG.y1 $ fretHeight 0
                , SVG.x2 $ halfStringWidth + state.x
                , SVG.y2 $ fretHeight state.numFrets
                , SVG.stroke "black"
                , SVG.class_ "string"
                ] : (renderPushedFret state.showColor state.x <$> A.fromFoldable frets) <> (renderFret state.x state.onLeft state.onRight `A.concatMap` A.range 0 (state.numFrets + 1))

getNotePositions :: GuitarString -> Note -> Note -> List FrettedNote
getNotePositions string root note =
  let fret = (findFret string note).position
      fromRoot = N.noteDistance root note
  in L.fromFoldable [ FrettedNote { fret: fret, fromRoot: Just fromRoot}
                    , FrettedNote { fret: fret + 12, fromRoot: Just fromRoot }
                    , FrettedNote { fret: fret + 24, fromRoot: Just fromRoot }
                    ]

eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval = case _ of
  PushFret fret next -> do
    frets <- H.gets (_.frets)
    let
      frettedNote = FrettedNote { fret: fret.position, fromRoot: Nothing }
      newFrets = S.insert frettedNote frets
    H.modify_ (_ { frets = newFrets })
    pure $ next unit
  ReleaseFret fret next -> do
    frets <- H.gets (_.frets)
    let
      frettedNote = FrettedNote { fret: fret.position, fromRoot: Nothing }
      newFrets = S.delete frettedNote frets
    H.modify_ (_ { frets = newFrets })
    pure $ next unit
  PushChord chord next -> do
    state <- H.get
    let
      newFrets = S.fromFoldable $ (getNotePositions state.string chord.rootNote) `L.concatMap` (L.fromFoldable chord.chord)
      forState = S.union newFrets state.frets
    H.modify_ (_ { frets = forState })
    pure $ next unit
  ReleaseChord chord next -> do
    state <- H.get
    let
      newFrets = S.fromFoldable $ (getNotePositions state.string chord.rootNote) `L.concatMap` (L.fromFoldable chord.chord)
      forState = S.difference state.frets newFrets
    H.modify_ (_ { frets = forState })
    pure $ next unit
  ShowColor showColor next -> do
    H.modify_ (_ {showColor = showColor})
    pure $ next unit
  Reset next -> do
    let
      frets :: Set FrettedNote
      frets = S.empty
    H.modify_ (_ { frets = frets })
    pure $ next unit
