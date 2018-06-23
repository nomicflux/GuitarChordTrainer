module Component.GuitarString where

import Prelude

import Component.SVG as SVG
import Data.Array ((:))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import GuitarString (GuitarString, Fret)
import Halogen as H
import Halogen.HTML as HH

type Input = { string :: GuitarString
             , fretsSize :: Int
             }

type State = { string :: GuitarString
             , frets :: Set Int
             , fretsSize :: Int
             }

initialState :: Input -> State
initialState input = { string: input.string
                     , frets: S.empty
                     , fretsSize: input.fretsSize
                     }

data Query a = PushFret Fret a | ReleaseFret Fret a

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
  { initialState: initialState
  , render
  , eval
  , receiver: const Nothing
  }

fretSize :: Int
fretSize = 20

halfFretSize :: Int
halfFretSize = fretSize / 2

halfWidth :: Int
halfWidth = 10

renderPushedFret :: forall p i. Int -> HH.HTML p i
renderPushedFret fret = SVG.circle [ SVG.cx halfWidth
                                   , SVG.cy $ fret * fretSize - halfFretSize
                                   , SVG.r 3
                                   , SVG.stroke "black"
                                   , SVG.fill "black"
                                   ]

renderFret :: forall p i. Int -> HH.HTML p i
renderFret fret = SVG.line [ SVG.x1 0
                           , SVG.x2 $ halfWidth * 2
                           , SVG.y1 $ fret * fretSize
                           , SVG.y2 $ fret * fretSize
                           , SVG.stroke "black"
                           ]

renderString :: forall p i. Int -> Set Int -> Array (HH.HTML p i)
renderString fretsSize frets =
  let stringLength = (fretsSize + 1) * fretSize
  in
   SVG.line [ SVG.x1 halfWidth
            , SVG.y1 0
            , SVG.x2 halfWidth
            , SVG.y2 stringLength
            , SVG.stroke "black"
            ] : (renderPushedFret <$> A.fromFoldable frets) <> (renderFret <$> A.range 0 (fretsSize + 1))


render :: State -> H.ComponentHTML Query
render state =
   SVG.svg [ SVG.width (2 * halfWidth)
           , SVG.height $ (state.fretsSize + 1) * fretSize
           ] (renderString state.fretsSize state.frets)

eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval = case _ of
  PushFret fret next -> do
    frets <- H.gets (_.frets)
    let newFrets = S.insert fret.position frets
    H.modify_ (_ { frets = newFrets })
    pure next
  ReleaseFret fret next -> do
    frets <- H.gets (_.frets)
    let newFrets = S.delete fret.position frets
    H.modify_ (_ { frets = newFrets })
    pure next
