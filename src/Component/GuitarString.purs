module Component.GuitarString where

import Prelude

import Chord (ThisChord)
import Component.Constants (fretHeight, fretWidth, halfFretWidth, halfStringWidth, stringLength)
import Component.SVG as SVG
import Data.Array ((:))
import Data.Array as A
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import GuitarString (Fret, GuitarString, findFret)
import Halogen as H
import Halogen.HTML as HH
import Note (Note)

type Input = { string :: GuitarString
             , numFrets :: Int
             , x :: Int
             , onLeft :: Boolean
             , onRight :: Boolean
             }

type State = { string :: GuitarString
             , frets :: Set Int
             , rootFrets :: Set Int
             , numFrets :: Int
             , x :: Int
             , onLeft :: Boolean
             , onRight :: Boolean
             }

initialState :: Input -> State
initialState input = { string: input.string
                     , frets: S.empty
                     , rootFrets: S.empty
                     , numFrets: input.numFrets
                     , x: input.x
                     , onLeft: input.onLeft
                     , onRight: input.onRight
                     }

data Query a = PushFret Fret (Unit -> a)
             | ReleaseFret Fret (Unit -> a)
             | PushChord ThisChord (Unit -> a)
             | ReleaseChord ThisChord (Unit -> a)
             | Reset (Unit -> a)

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
  { initialState: initialState
  , render
  , eval
  , receiver: const Nothing
  }

fretColor :: Int -> Set Int -> String
fretColor fret root =
  if S.member fret root then "red" else "black"

renderPushedFret :: forall p i. Int -> Set Int -> Int -> HH.HTML p i
renderPushedFret x root fret = SVG.circle [ SVG.cx $ halfStringWidth + x
                                     , SVG.cy $ fretHeight fret - halfFretWidth
                                     , SVG.r 3
                                     , SVG.stroke "black"
                                     , SVG.fill $ fretColor fret root
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
     renderString :: forall p i. Set Int -> Array (HH.HTML p i)
     renderString frets =
       SVG.line [ SVG.x1 $ halfStringWidth + state.x
                , SVG.y1 $ fretHeight 0
                , SVG.x2 $ halfStringWidth + state.x
                , SVG.y2 $ fretHeight state.numFrets
                , SVG.stroke "black"
                , SVG.class_ "string"
                ] : (renderPushedFret state.x state.rootFrets <$> A.fromFoldable frets) <> (renderFret state.x state.onLeft state.onRight `A.concatMap` A.range 0 (state.numFrets + 1))

getNotePositions :: GuitarString -> Note -> List Int
getNotePositions string note =
  let fret = (findFret string note).position
  in L.fromFoldable [fret, fret + 12, fret + 24]

eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval = case _ of
  PushFret fret next -> do
    frets <- H.gets (_.frets)
    let newFrets = S.insert fret.position frets
    H.modify_ (_ { frets = newFrets })
    pure $ next unit
  ReleaseFret fret next -> do
    frets <- H.gets (_.frets)
    let newFrets = S.delete fret.position frets
    H.modify_ (_ { frets = newFrets })
    pure $ next unit
  PushChord chord next -> do
    state <- H.get
    let
      newFrets = S.fromFoldable $ (getNotePositions state.string) `L.concatMap` (L.fromFoldable chord.chord)
      rootFrets = S.fromFoldable $ getNotePositions state.string chord.rootNote
      forState = S.union newFrets state.frets
    H.modify_ (_ { frets = forState, rootFrets = rootFrets })
    pure $ next unit
  ReleaseChord chord next -> do
    state <- H.get
    let
      newFrets = S.fromFoldable $ (getNotePositions state.string) `L.concatMap` (L.fromFoldable chord.chord)
      forState = S.difference state.frets newFrets
      rootFrets :: Set Int
      rootFrets = S.empty
    H.modify_ (_ { frets = forState, rootFrets = rootFrets })
    pure $ next unit
  Reset next -> do
    let
      frets :: Set Int
      frets = S.empty
      rootFrets :: Set Int
      rootFrets = S.empty
    H.modify_ (_ { frets = frets, rootFrets = rootFrets })
    pure $ next unit
