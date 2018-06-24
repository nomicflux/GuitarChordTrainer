module Component.App where

import Prelude

import Chord (Chord(..), ThisChord)
import Chord as C
import Component.Guitar as CG
import Data.Array ((!!))
import Data.Array as A
import Data.Foldable as F
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Guitar (Guitar)
import Guitar as G
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Note (Note)
import Note as N
import Tagged as T
import Web.Event.Event as ET

type State = { currentGuitar :: Guitar
             , currentChord :: Chord
             , currentNote :: Note
             , slot :: String
             }

initialState :: State
initialState =
  let mguitar = G.allGuitars !! 0
  in
   { currentGuitar: maybe G.standardGuitar T.getValue mguitar
   , currentChord: maybe C.majorChord T.getValue (C.allChords !! 0)
   , currentNote: maybe N.A T.getValue (N.allNotes !! 0)
   , slot: maybe "" T.getName mguitar
   }

data Query a = ChangeGuitar String a
             | ChangeChord String a
             | ChangeNote String a
             | HandleGuitar a

data Slot = Slot String
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }

render :: forall m. State -> H.ParentHTML Query CG.Query Slot m
render state =
  HH.div [ HP.class_ $ HH.ClassName "pure-g" ]
  [ renderSidebar
  ,  HH.div [ HP.class_ $ HH.ClassName "pure-u-3-4" ]
     [ HH.slot (Slot state.slot) CG.component state.currentGuitar $ HE.input_ HandleGuitar ]
  ]
  where
    renderSidebar :: H.ParentHTML Query CG.Query Slot m
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName "pure-u pure-u-1-4" ]
      [ HH.form [ HP.class_ $ HH.ClassName "pure-form" ]
        [ mkSelect "Guitar" guitarMap (Just state.slot) ChangeGuitar
        , mkSelect "Chord" chordMap Nothing ChangeChord
        , mkSelect "Note" noteMap Nothing ChangeNote
        ]
      ]

    mkOption :: forall p i. String -> HH.HTML p i
    mkOption k = HH.option [HP.value k] [ HH.text k ]

    mkSelect :: forall v. String ->
                Map String v ->
                Maybe String ->
                (String -> Unit -> Query Unit) ->
                H.ParentHTML Query CG.Query Slot m
    mkSelect label items value query =
      let
        keys = A.fromFoldable $ M.keys items
      in
       HH.div_
       [ HH.label [HP.for label] [ HH.text $ label <> ": " ]
       , HH.select ([ HP.name label
                    , HE.onValueChange (HE.input query)
                    ] <> (maybe [] (\v -> [HP.value v]) value)
)         (mkOption <$> keys)
       ]

guitarMap :: Map String Guitar
guitarMap = T.taggedToMap G.allGuitars

chordMap :: Map String Chord
chordMap = T.taggedToMap C.allChords

noteMap :: Map String Note
noteMap = T.taggedToMap N.allNotes

eval :: forall m. Query ~> H.ParentDSL State Query CG.Query Slot Void m
eval = case _ of
  ChangeGuitar name next -> do
    let mguitar = M.lookup name guitarMap
    maybe (pure next) (\guitar -> do
                          H.modify_ (_ { currentGuitar = guitar, slot = name })
                          note <- H.gets (_.currentNote)
                          chord <- H.gets (_.currentChord)
                          let thisChord = C.generateChord chord note
                          _ <- H.query (Slot name) $ H.request (CG.ShowChord thisChord)
                          pure next) mguitar
  ChangeChord name next -> do
    let mchord = M.lookup name chordMap
    maybe (pure next) (\chord -> do
                          H.modify_ (_ { currentChord = chord })
                          note <- H.gets (_.currentNote)
                          slot <- H.gets (_.slot)
                          let thisChord = C.generateChord chord note
                          _ <- H.query (Slot slot) $ H.request (CG.ShowChord thisChord)
                          pure next) mchord
  ChangeNote name next -> do
    let mnote = M.lookup name noteMap
    maybe (pure next) (\note -> do
                          H.modify_ (_ { currentNote = note })
                          chord <- H.gets (_.currentChord)
                          slot <- H.gets (_.slot)
                          let thisChord = C.generateChord chord note
                          _ <- H.query (Slot slot) $ H.request (CG.ShowChord thisChord)
                          pure next) mnote
  HandleGuitar next ->
    pure next
