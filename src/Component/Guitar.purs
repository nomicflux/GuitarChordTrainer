module Component.Guitar where

import Prelude

import Chord (ThisChord)
import Component.GuitarString as GS
import Component.SVG as SVG
import Data.Array as A
import Data.Maybe (Maybe(..))
import Guitar (Guitar, standardGuitar)
import GuitarString (GuitarString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a = AddChord ThisChord a | HandleString Int GuitarString a

data Slot = Slot Int

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State = { guitar :: Guitar
             , fretsSize :: Int
             }

initialState :: State
initialState = { guitar: standardGuitar
               , fretsSize: 12
               }

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }

renderString :: forall m. Int -> Int -> GuitarString -> H.ParentHTML Query GS.Query Slot m
renderString fretsSize id string =
  let
    stringInput = { string
                  , fretsSize
                  }
  in HH.slot (Slot id) GS.component stringInput (HE.input_ $ HandleString id string)

render :: forall m. State -> H.ParentHTML Query GS.Query Slot m
render state =
  HH.div_ (A.mapWithIndex (renderString state.fretsSize) state.guitar.strings)

eval :: forall m. Query ~> H.ParentDSL State Query GS.Query Slot Void m
eval = case _ of
  AddChord chord next -> do
    pure next
  HandleString id string next -> do
    pure next
