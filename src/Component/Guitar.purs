module Component.Guitar where

import Prelude

import Chord (ThisChord)
import Component.Constants (fretHeight, fretMarkerRadius, fretWidth, halfFretHeight, lineHeight, stringLength)
import Component.GuitarString as GS
import Component.SVG as SVG
import Data.Array as A
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as S
import Effect.Aff (Aff)
import Guitar (Guitar)
import GuitarString (GuitarString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Note (Note)
import Web.HTML.HTMLElement as DOM
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

data Query a = ShowChord ThisChord (Unit -> a)
             | ShowColor Boolean (Unit -> a)
             | ClickFret MouseEvent a
             | ClearChord (Unit -> a)
             | ClearAll (Unit -> a)
             | GetNotes (Set Note -> a)
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

data Message = Selected (Set Note)

component :: H.Component HH.HTML Query Guitar Message Aff
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
             , SVG.cy $ lineHeight fret - halfFretHeight
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

containerRef :: H.RefLabel
containerRef = H.RefLabel "GuitarContainer"

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
   HH.div [ HP.ref containerRef ]
   [ SVG.svg [ SVG.height height
             , SVG.width width
             , SVG.viewBox $ A.intercalate " " [ "0 0"
                                               , show width
                                               , show $ height + 1
                                               ]
             , HE.onClick $ HE.input ClickFret
             ] $ renderedKeyFrets <> renderedOctaveFrets <> renderedStrings
   ]

getSlotIds :: State -> Array Int
getSlotIds state =
  let numStrings = A.length state.guitar.strings
  in A.range 0 $ numStrings - 1

eval :: Query ~> H.ParentDSL State Query GS.Query Slot Message Aff
eval = case _ of
  ShowChord chord next -> do
    ids <- H.gets getSlotIds
    let reset _ id = H.query (Slot id) $ H.request GS.ResetChord
        pushNotes _ id = H.query (Slot id) $ H.request (GS.PushChord chord)
    _ <- A.foldM reset Nothing ids
    _ <- A.foldM pushNotes Nothing ids
    pure $ next unit
  ShowColor showColor reply -> do
    ids <- H.gets getSlotIds
    let toggleColor _ id = H.query (Slot id) $ H.request (GS.ShowColor showColor)
    _ <- A.foldM toggleColor Nothing ids
    pure $ reply unit
  ClickFret event next -> do
    H.getHTMLElementRef containerRef >>= case _ of
      Nothing -> pure unit
      Just el -> do
        offsetLeft <- H.liftEffect do
          rect <- DOM.getBoundingClientRect el
          pure $ rect.left
        let
          x = (toNumber $ ME.pageX event) - offsetLeft
          y = ME.pageY event
          string = round $ x / (toNumber fretWidth) - 0.5
          fret = round $ (toNumber y) / (toNumber fretHeight) - 1.0
        _ <- H.query (Slot string) $ H.request (GS.ToggleFret fret)
        ids <- H.gets getSlotIds
        let notes acc id = do
              these <- H.query (Slot id) $ H.request GS.GetNotes
              pure $ S.union acc (fromMaybe S.empty these)
        allNotes <- A.foldM notes S.empty ids
        H.raise (Selected allNotes)
        pure unit
    pure next
  ClearChord next -> do
    ids <- H.gets getSlotIds
    let reset _ id = H.query (Slot id) $ H.request GS.ResetChord
    _ <- A.foldM reset Nothing ids
    pure $ next unit
  ClearAll next -> do
    ids <- H.gets getSlotIds
    let resetChord _ id = H.query (Slot id) $ H.request GS.ResetChord
        resetToggled _ id = H.query (Slot id) $ H.request GS.ResetToggled
    _ <- A.foldM resetChord Nothing ids
    _ <- A.foldM resetToggled Nothing ids
    pure $ next unit
  GetNotes reply -> do
    ids <- H.gets getSlotIds
    let notes acc id = do
          these <- H.query (Slot id) $ H.request GS.GetNotes
          pure $ S.union acc (fromMaybe S.empty these)
    allNotes <- A.foldM notes S.empty ids
    pure $ reply allNotes
  HandleString id string next -> do
    pure next
