module Component.Guitar where

import Prelude

import Component.Common.Communication (passAlong, getBack)
import Component.Common.Constants (fretHeight, fretMarkerRadius, fretWidth, halfFretHeight, lineHeight, stringLength)
import Component.Common.Offset (getOffset)
import Component.Common.RootedInterval (RootedInterval)
import Component.Common.SVG as SVG
import Component.GuitarString as GS
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
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

data Query a = ShowNotes RootedInterval (Unit -> a)
             | ShowColor Boolean (Unit -> a)
             | ClickFret MouseEvent a
             | ClearNotes (Unit -> a)
             | ClearToggled (Unit -> a)
             | ClearAll (Unit -> a)
             | GetNotes (Set Note -> a)
             | SelectNotes (Set Note) (Unit -> a)
             | HandleString Int GuitarString a

data Slot = Slot Int
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State = { guitar :: Guitar
             , numFrets :: Int
             , scrollY :: Int
             }

initialState :: Guitar -> State
initialState = { guitar: _
               , numFrets: 24
               , scrollY: 0
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
  SVG.circle [ SVG.r radius
             , SVG.class_ "fret-circle"
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
   HH.div [ HP.class_ $ HH.ClassName "guitar"
          , HP.ref containerRef
          ]
   [ SVG.svg [ SVG.height height
             , SVG.width width
             , SVG.viewBox $ A.intercalate " " [ "0 0"
                                               , show width
                                               , show $ height + 1
                                               ]
             , HE.onClick $ HE.input ClickFret
             ] $ renderedKeyFrets <> renderedOctaveFrets <> renderedStrings
   ]

eval :: Query ~> H.ParentDSL State Query GS.Query Slot Message Aff
eval (ShowNotes intervals next) = do
  _ <- passAlong GS.ResetNotes
  _ <- passAlong (GS.PushNotes intervals)
  pure $ next unit
eval (ShowColor showColor reply) = do
  _ <- passAlong (GS.ShowColor showColor)
  pure $ reply unit
eval (ClickFret event next) =
  H.getRef containerRef >>= case _ of
    Nothing -> pure next
    Just el -> do
      offset <- H.liftEffect (getOffset el)
      let
        x = (toNumber $ ME.clientX event) - offset.left
        y = (toNumber $ ME.clientY event) - offset.top
        string = round $ x / (toNumber fretWidth) - 0.5
        fret = round $ y / (toNumber fretHeight) - 0.5
      _ <- H.query (Slot string) $ H.request (GS.ToggleFret fret)
      allNotes <- getBack (\acc these -> S.union acc (fromMaybe S.empty these)) S.empty GS.GetNotes
      H.raise (Selected allNotes)
      pure next
eval (ClearNotes next) = do
  _ <- passAlong GS.ResetNotes
  pure $ next unit
eval (ClearToggled next) = do
  _ <- passAlong GS.ResetToggled
  pure $ next unit
eval (ClearAll next) = do
  _ <- passAlong GS.ResetNotes
  _ <- passAlong GS.ResetToggled
  pure $ next unit
eval (SelectNotes notes reply) = do
  _ <- passAlong (GS.SelectNotes notes)
  pure $ reply unit
eval (GetNotes reply) = do
  allNotes <- getBack (\acc these -> S.union acc (fromMaybe S.empty these)) S.empty GS.GetNotes
  pure $ reply allNotes
eval (HandleString id string next) = pure next
