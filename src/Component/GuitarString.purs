module Component.GuitarString where

import Prelude

import Component.Common.Constants (halfFretHeight, halfFretWidth, lineHeight, pushedFretRadius)
import Component.Common.FretColor (fretColor)
import Component.Common.RootedInterval (RootedInterval)
import Component.Common.SVG as SVG
import Data.Array ((:))
import Data.Array as A
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as S
import GuitarString (GuitarString, findFret)
import Halogen as H
import Halogen.HTML as HH
import Note (Note)
import Note as N

data FrettedNote = FrettedNote { fret :: Int
                               , fromRoot :: Maybe Int
                               }
derive instance eqFrettedNote :: Eq FrettedNote
instance ordFrettedNote :: Ord FrettedNote where
  compare (FrettedNote a) (FrettedNote b) =
    if a.fret == b.fret
    then a.fromRoot `compare` b.fromRoot
    else a.fret `compare` b.fret

getFret :: FrettedNote -> Int
getFret (FrettedNote fret) = fret.fret

getInterval :: FrettedNote -> Maybe Int
getInterval (FrettedNote fret) = fret.fromRoot

type Input = { string :: GuitarString
             , numFrets :: Int
             , x :: Int
             , onLeft :: Boolean
             , onRight :: Boolean
             }

type State = { string :: GuitarString
             , frets :: Set FrettedNote
             , toggled :: Set Int
             , numFrets :: Int
             , x :: Int
             , onLeft :: Boolean
             , onRight :: Boolean
             , showColor :: Boolean
             }

initialState :: Input -> State
initialState input = { string: input.string
                     , frets: S.empty
                     , toggled: S.empty
                     , numFrets: input.numFrets
                     , x: input.x
                     , onLeft: input.onLeft
                     , onRight: input.onRight
                     , showColor: true
                     }

data Query a = ToggleFret Int (Unit -> a)
             | PushNotes RootedInterval (Unit -> a)
             | ShowColor Boolean (Unit -> a)
             | GetNotes (Set Note -> a)
             | SelectNotes (Set Note) (Unit -> a)
             | ResetNotes (Unit -> a)
             | ResetToggled (Unit -> a)

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

renderFret :: forall p i. Int -> Boolean -> Boolean -> Int -> Array (HH.HTML p i)
renderFret x onLeft onRight fret =
  let left = if onLeft then [renderLeftFret x fret] else []
      right = if onRight then [renderRightFret x fret] else []
  in left <> right

renderLeftFret :: forall p i. Int -> Int -> HH.HTML p i
renderLeftFret x fret = SVG.line [ SVG.x1 $ x
                                 , SVG.x2 $ halfFretWidth + x
                                 , SVG.y1 $ lineHeight fret
                                 , SVG.y2 $ lineHeight fret
                                 , SVG.stroke "black"
                                 , SVG.class_ "fret"
                                 ]

renderRightFret :: forall p i. Int -> Int -> HH.HTML p i
renderRightFret x fret = SVG.line [ SVG.x1 $ halfFretWidth + x
                                  , SVG.x2 $ halfFretWidth * 2 + x
                                  , SVG.y1 $ lineHeight fret
                                  , SVG.y2 $ lineHeight fret
                                  , SVG.stroke "black"
                                  , SVG.class_ "fret"
                                  ]

render :: State -> H.ComponentHTML Query
render state =
   SVG.svg [ ] (renderString state.frets)
   where
     renderString :: forall p i. Set FrettedNote -> Array (HH.HTML p i)
     renderString frets =
       SVG.line [ SVG.x1 $ halfFretWidth + state.x
                , SVG.y1 $ lineHeight 0
                , SVG.x2 $ halfFretWidth + state.x
                , SVG.y2 $ lineHeight state.numFrets
                , SVG.stroke "black"
                , SVG.class_ "string"
                ] : (renderPushedFret <$> A.fromFoldable frets) <> (renderToggledFret <$> A.fromFoldable state.toggled) <> (renderFret state.x state.onLeft state.onRight `A.concatMap` A.range 0 (state.numFrets + 1))

     renderToggledFret :: forall p i. Int -> HH.HTML p i
     renderToggledFret fretPosition =
        SVG.circle [ SVG.cx $ halfFretWidth + state.x
                   , SVG.cy $ lineHeight fretPosition - halfFretHeight
                   , SVG.r pushedFretRadius
                   , SVG.class_ "toggled-fret"
                   ]

     renderPushedFret :: forall p i. FrettedNote -> HH.HTML p i
     renderPushedFret (FrettedNote fret) =
       let
         color = if state.showColor then fretColor else fretColorLess
       in
        SVG.circle [ SVG.cx $ halfFretWidth + state.x
                   , SVG.cy $ lineHeight fret.fret - halfFretHeight
                   , SVG.r pushedFretRadius
                   , SVG.fill $ maybe "black" color fret.fromRoot
                   , SVG.class_ "pushed-fret"
                   ]

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
  ToggleFret fretPosition next -> do
    state <- H.get
    let
      present = S.member fretPosition state.toggled
      newToggled = if present
                   then S.delete fretPosition state.toggled
                   else S.insert fretPosition state.toggled
    H.modify_ (_ { toggled = newToggled })
    pure $ next unit
  PushNotes intervals next -> do
    state <- H.get
    let
      newFrets = S.fromFoldable $ (getNotePositions state.string intervals.rootNote) `L.concatMap` (L.fromFoldable intervals.notes)
      forState = S.union newFrets state.frets
    H.modify_ (_ { frets = forState })
    pure $ next unit
  ShowColor showColor next -> do
    H.modify_ (_ {showColor = showColor})
    pure $ next unit
  GetNotes reply -> do
    state <- H.get
    let notes = S.map (\fret -> N.incNoteBy state.string.baseNote fret) state.toggled
    pure $ reply notes
  SelectNotes notes reply -> do
    state <- H.get
    let
      positions = L.concatMap ((map getFret) <<< getNotePositions state.string state.string.baseNote) (L.fromFoldable notes)
    H.modify_ (_ { toggled = S.fromFoldable positions })
    pure $ reply unit
  ResetNotes next -> do
    H.modify_ (_ { frets = S.empty :: Set FrettedNote })
    pure $ next unit
  ResetToggled next -> do
    H.modify_ (_ { toggled = S.empty :: Set Int })
    pure $ next unit
