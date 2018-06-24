module Component.Guitar where

import Prelude

import Chord (ThisChord)
import Chord as C
import Component.Constants (fretHeight, fretMarkerRadius, fretWidth, halfFretWidth, stringLength)
import Component.GuitarString as GS
import Component.SVG as SVG
import Data.Array as A
import Data.Maybe (Maybe(..))
import Guitar (Guitar)
import GuitarString (GuitarString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Note as N

data Query a = ShowChord ThisChord (Unit -> a)
             | ShowColor Boolean (Unit -> a)
             | HandleString Int GuitarString a

data Slot = Slot Int

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State = { guitar :: Guitar
             , numFrets :: Int
             }

initialState :: Guitar -> State
initialState = { guitar: _
               , numFrets: 24
               }

data Message = NoOp

component :: forall m. H.Component HH.HTML Query Guitar Message m
component =
  H.parentComponent
  { initialState: initialState
  , render
  , eval
  , receiver: const Nothing
  }

renderString :: forall m. Int -> Int -> Int -> GuitarString -> H.ParentHTML Query GS.Query Slot m
renderString numFrets numStrings id string =
  let
    stringInput = { string
                  , numFrets
                  , x: id * fretWidth
                  , onLeft: id /= 0
                  , onRight: id /= (numStrings - 1)
                  }
  in HH.slot (Slot id) GS.component stringInput (HE.input_ $ HandleString id string)

renderFretCircle :: forall p i. Int -> Int -> Int -> Int -> HH.HTML p i
renderFretCircle numStrings fret radius offset =
  SVG.circle [ SVG.fill "grey"
             , SVG.stroke "grey"
             , SVG.r radius
             , SVG.cx $ numStrings * fretWidth / 2 + offset
             , SVG.cy $ fretHeight fret - halfFretWidth
             ]

octaveFrets :: Array Int
octaveFrets = [12, 24]

renderOctaveFret :: forall p i. Int -> Int -> Array (HH.HTML p i)
renderOctaveFret numStrings fret =
  let spacing = fretMarkerRadius * 3 / 2
  in
   [ renderFretCircle numStrings fret fretMarkerRadius (-spacing)
   , renderFretCircle numStrings fret fretMarkerRadius spacing
   ]

keyFrets :: Array Int
keyFrets = [3, 5, 7, 9, 15, 17, 19, 21]

renderKeyFret :: forall p i. Int -> Int -> HH.HTML p i
renderKeyFret numStrings fret = renderFretCircle numStrings fret fretMarkerRadius 0

render :: forall m. State -> H.ParentHTML Query GS.Query Slot m
render state =
  let
    numStrings = A.length state.guitar.strings
    height = (stringLength state.numFrets) + 1
    width = numStrings * fretWidth
    renderedKeyFrets = renderKeyFret numStrings <$> keyFrets
    renderedOctaveFrets = renderOctaveFret numStrings `A.concatMap` octaveFrets
    renderedStrings = A.mapWithIndex (renderString state.numFrets numStrings) state.guitar.strings
  in
   HH.div_
   [ SVG.svg [ SVG.height height
             , SVG.width width
             , SVG.viewBox $ A.intercalate " " [ "0 0"
                                               , show width
                                               , show $ height + 1
                                               ]
             ] $ renderedKeyFrets <> renderedOctaveFrets <> renderedStrings
   ]

getSlotIds :: State -> Array Int
getSlotIds state =
  let numStrings = A.length state.guitar.strings
  in A.range 0 $ numStrings - 1

eval :: forall m. Query ~> H.ParentDSL State Query GS.Query Slot Message m
eval = case _ of
  ShowChord chord next -> do
    ids <- H.gets getSlotIds
    let reset _ id = H.query (Slot id) $ H.request GS.Reset
        pushNotes _ id = H.query (Slot id) $ H.request (GS.PushChord chord)
    _ <- A.foldM reset Nothing ids
    _ <- A.foldM pushNotes Nothing ids
    pure $ next unit
  ShowColor showColor reply -> do
    ids <- H.gets getSlotIds
    let toggleColor _ id = H.query (Slot id) $ H.request (GS.ShowColor showColor)
    _ <- A.foldM toggleColor Nothing ids
    pure $ reply unit
  HandleString id string next -> do
    pure next
