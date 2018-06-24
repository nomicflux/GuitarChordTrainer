module Component.App where

import Prelude

import Chord (Chord, IntervalledNote, ThisChord)
import Chord as C
import Chord as I
import Component.Constants (pushedFretRadius)
import Component.FretColor (fretColor)
import Component.Guitar as CG
import Component.SVG as SVG
import Data.Array ((!!))
import Data.Array as A
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as S
import Guitar (Guitar)
import Guitar as G
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Interval (intervalToName)
import Note (Note)
import Note as N
import Tagged as T

type State = { currentGuitar :: Guitar
             , currentChord :: Maybe Chord
             , currentNote :: Maybe Note
             , slot :: String
             , showColor :: Boolean
             , filteredNotes :: Set Note
             }

emptyFilter :: Set Note
emptyFilter = S.empty

initialState :: State
initialState =
  let mguitar = G.allGuitars !! 0
  in
   { currentGuitar: maybe G.standardGuitar T.getValue mguitar
   , currentChord: Nothing
   , currentNote: Nothing
   , slot: maybe "" T.getName mguitar
   , showColor: true
   , filteredNotes: emptyFilter
   }

getChord :: State -> Maybe ThisChord
getChord state = do
  chord <- state.currentChord
  note <- state.currentNote
  pure $ C.generateChord chord note

getFilteredChord :: State -> Maybe ThisChord
getFilteredChord state =
  (flip C.filterNotes) state.filteredNotes <$> getChord state

data Query a = ChangeGuitar String a
             | ChangeChord String a
             | ChangeNote String a
             | ToggleShowColor a
             | ToggleNote Note a
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

isFiltered :: State -> Note -> Boolean
isFiltered state note = S.member note state.filteredNotes

render :: forall m. State -> H.ParentHTML Query CG.Query Slot m
render state =
  HH.div [ HP.class_ $ HH.ClassName "pure-g" ]
  [ renderSidebar
  ,  HH.div [ HP.class_ $ HH.ClassName "pure-u-1 pure-u-md-1-2 pure-u-lg-2-3" ]
     [ HH.slot (Slot state.slot) CG.component state.currentGuitar $ HE.input_ HandleGuitar ]
  ]
  where
    renderSidebar :: H.ParentHTML Query CG.Query Slot m
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-1 pure-u-sm-1 pure-u-md-1-2 pure-u-lg-1-3" ]
      [ HH.form [ HP.class_ $ HH.ClassName "pure-form" ] $
        [ mkSelect "Tuning" guitarMap (Just state.slot) ChangeGuitar
        , mkSelect "Chord" chordMap Nothing ChangeChord
        , mkSelect "Note" noteMap Nothing ChangeNote
        , mkButton ((if state.showColor then "Hide" else "Show") <> " Interval Colors") "plain" ToggleShowColor
        ] <> maybe [] (A.singleton <<< renderIntervalChart <<< C.chordToIntervals) (getChord state)
      ]

    renderInterval :: IntervalledNote ->
                      H.ParentHTML Query CG.Query Slot m
    renderInterval interval =
      let halfWidth = pushedFretRadius + 1
          halfHeight = pushedFretRadius + 1
          class_ = if isFiltered state (I.getNote interval) then "filtered" else "unfiltered"
          filterInterval x = state.showColor || (I.getInterval x) == 0
          f i = if filterInterval i
                then fretColor (I.getInterval i)
                else "black"
      in
       HH.li
       [ HP.class_ $ HH.ClassName ("pure-menu-item restricted-height interval-key " <> class_)
       , HE.onClick $ HE.input_ (ToggleNote (I.getNote interval))
       ]
       [ SVG.svg [ SVG.width $ halfWidth * 2
                 , SVG.height $ halfHeight * 2
                 , SVG.viewBox $ A.intercalate " " [ "0 0"
                                                   , show $ halfWidth * 2
                                                   , show $ halfHeight * 2
                                                   ]
                 ] [ SVG.circle [ SVG.r pushedFretRadius
                                , SVG.cx halfWidth
                                , SVG.cy halfHeight
                                , SVG.fill $ f interval
                                , SVG.stroke "black"
                                ]
                   ]
       , HH.text $ " = " <> intervalToName (I.getInterval interval)
       ]

    renderIntervalChart :: Set IntervalledNote ->
                           H.ParentHTML Query CG.Query Slot m
    renderIntervalChart intervals =
      let aIntervals = A.fromFoldable intervals
      in
       HH.div [ HP.class_ $ HH.ClassName "pure-menu restricted-width"]
       [ HH.ul [ HP.class_ $ HH.ClassName "pure-menu-list"]
         (renderInterval <$> aIntervals)
       ]

    mkButton :: String -> String -> (Unit -> Query Unit) -> H.ParentHTML Query CG.Query Slot m
    mkButton text class_ query =
      HH.div_
      [ HH.button [ HP.class_ $ HH.ClassName ("pure-button button-" <> class_)
                  , HE.onClick $ HE.input_ query
                  , HP.type_ $ HP.ButtonButton
                  ]
        [ HH.text text ]
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
       HH.div [ HP.class_ $ HH.ClassName "gct-select-div" ]
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
                          H.modify_ (_ { currentGuitar = guitar
                                       , slot = name
                                       })
                          thisChord <- H.gets getFilteredChord
                          case thisChord of
                            Just chord ->  do
                              _ <- H.query (Slot name) $ H.request (CG.ShowChord chord)
                              pure next
                            Nothing -> pure next
                      ) mguitar
  ChangeChord name next -> do
    case M.lookup name chordMap of
      Nothing -> pure next
      Just newChord -> do
        H.modify_ (_ { currentChord = Just newChord
                     , filteredNotes = emptyFilter
                     })
        thisChord <- H.gets getChord
        case thisChord of
          Nothing -> pure next
          Just chord -> do
            slot <- H.gets (_.slot)
            _ <- H.query (Slot slot) $ H.request (CG.ShowChord chord)
            pure next
  ChangeNote name next -> do
    case M.lookup name noteMap of
      Nothing -> pure next
      Just newNote -> do
        oldNote <- H.gets (_.currentNote)
        filteredNotes <- H.gets (_.filteredNotes)
        let change = (flip N.noteDistance) newNote <$> oldNote
            newFilter =
              maybe emptyFilter (\d -> S.map ((flip N.incNoteBy) d) filteredNotes) change
        H.modify_ (_ { currentNote = Just newNote
                     , filteredNotes = newFilter
                     })
        thisChord <- H.gets getFilteredChord
        case thisChord of
          Nothing -> pure next
          Just chord -> do
            slot <- H.gets (_.slot)
            _ <- H.query (Slot slot) $ H.request (CG.ShowChord chord)
            pure next
  ToggleShowColor next -> do
    slot <- H.gets (_.slot)
    showColor <- H.gets (_.showColor)
    let nowShow = not showColor
    H.modify_ (_ { showColor = nowShow })
    _ <- H.query (Slot slot) $ H.request (CG.ShowColor nowShow)
    pure next
  ToggleNote note next -> do
    filteredNotes <- H.gets (_.filteredNotes)
    let newFilter = if S.member note filteredNotes
                    then S.delete note filteredNotes
                    else S.insert note filteredNotes
    H.modify_ (_ { filteredNotes = newFilter })
    thisChord <- H.gets getFilteredChord
    case thisChord of
      Just chord -> do
        slot <- H.gets (_.slot)
        _ <- H.query (Slot slot) $ H.request (CG.ShowChord chord)
        pure next
      Nothing -> pure next
  HandleGuitar next ->
    pure next
