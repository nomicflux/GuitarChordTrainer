module Component.App where

import Prelude

import Chord (Chord, IntervalledNote, ThisChord)
import Chord as C
import Chord as I
import Component.Constants (pushedFretRadius)
import Component.FretColor (fretColor)
import Component.Guitar as CG
import Component.SVG as SVG
import Data.Array ((!!), (:))
import Data.Array as A
import Data.List (List)
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
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
             , currentChord :: String
             , currentNote :: String
             , slot :: String
             , showColor :: Boolean
             , filteredIntervals :: Set Note
             , selectedNotes :: Set Note
             }

defaultOptionValue :: String
defaultOptionValue = ""

emptyFilter :: Set Note
emptyFilter = S.empty

initialState :: State
initialState =
  let mguitar = G.allGuitars !! 0
  in
   { currentGuitar: maybe G.standardGuitar T.getValue mguitar
   , currentChord: defaultOptionValue
   , currentNote: defaultOptionValue
   , slot: maybe defaultOptionValue T.getName mguitar
   , showColor: true
   , filteredIntervals: emptyFilter
   , selectedNotes: emptyFilter
   }

getNote :: State -> Maybe Note
getNote state = M.lookup state.currentNote noteMap

getChord :: State -> Maybe ThisChord
getChord state = do
  chord <- M.lookup state.currentChord chordMap
  note <- M.lookup state.currentNote noteMap
  pure $ C.generateChord chord note

getFilteredChord :: State -> Maybe ThisChord
getFilteredChord state =
  (flip C.filterNotes) state.filteredIntervals <$> getChord state

data Query a = ChangeGuitar String a
             | ChangeChord String a
             | ChangeNote String a
             | ToggleShowColor a
             | ToggleInterval Note a
             | ClearSelected a
             | ClearAll a
             | HandleGuitar CG.Message a

data Slot = Slot String
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }

isFiltered :: State -> Note -> Boolean
isFiltered state note = S.member note state.filteredIntervals

isFit :: String -> String -> Set Note -> Boolean
isFit note chord allowed =
  let
    thisChord = M.lookup note allGenChords >>= M.lookup chord
  in
   maybe false (\c -> S.intersection allowed c.chord == allowed) thisChord

filteredChords :: String -> Set Note -> Set String
filteredChords currentNote allowedNotes =
  if S.isEmpty allowedNotes then M.keys chordMap
  else
    S.fromFoldable $ L.filter (\k -> isFit currentNote k allowedNotes) $ L.fromFoldable $ M.keys chordMap

filteredIntervals :: Set Note -> Set String
filteredIntervals allowedNotes =
  if S.isEmpty allowedNotes then M.keys noteMap
  else
    let
      allNotes = L.fromFoldable $ M.keys noteMap
      allowedChords n = L.fromFoldable $ filteredChords n allowedNotes
      noteChords :: String -> List ThisChord
      noteChords n =
        L.mapMaybe (\c -> M.lookup n allGenChords >>= M.lookup c) (allowedChords n)
      hasChords :: String -> Boolean
      hasChords n = (not L.null) (noteChords n)
    in
     S.fromFoldable $ L.filter hasChords $ L.fromFoldable $ M.keys noteMap

tuningRefName :: String
tuningRefName = "Tuning"

chordRefName :: String
chordRefName = "Chord"

noteRefName :: String
noteRefName = "Note"

render :: State -> H.ParentHTML Query CG.Query Slot Aff
render state =
  HH.div [ HP.class_ $ HH.ClassName "pure-g" ]
  [ renderSidebar
  ,  HH.div [ HP.class_ $ HH.ClassName "pure-u-1 pure-u-md-1-2 pure-u-lg-2-3" ]
     [ HH.slot (Slot state.slot) CG.component state.currentGuitar $ HE.input HandleGuitar ]
  ]
  where
    renderSidebar :: H.ParentHTML Query CG.Query Slot Aff
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName "pure-u-1 pure-u-sm-1 pure-u-md-1-2 pure-u-lg-1-3" ]
      [ HH.form [ HP.class_ $ HH.ClassName "pure-form" ] $
        [ mkSelect tuningRefName guitarMap (M.keys guitarMap) state.slot ChangeGuitar
        , mkSelect chordRefName chordMap (filteredChords state.currentNote state.selectedNotes) state.currentChord ChangeChord
        , mkSelect noteRefName noteMap (filteredIntervals state.selectedNotes) state.currentNote ChangeNote
        , mkButton ((if state.showColor then "Hide" else "Show") <> " Interval Colors") "plain" ToggleShowColor
        , mkButton "Clear Selected Frets" "warning" ClearSelected
        , mkButton "Clear All" "error" ClearAll
        ] <> maybe [] (A.singleton <<< renderIntervalChart <<< C.chordToIntervals) (getChord state)
      ]

    renderInterval :: IntervalledNote ->
                      H.ParentHTML Query CG.Query Slot Aff
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
       , HE.onClick $ HE.input_ (ToggleInterval (I.getNote interval))
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
                           H.ParentHTML Query CG.Query Slot Aff
    renderIntervalChart intervals =
      let aIntervals = A.fromFoldable intervals
      in
       HH.div [ HP.class_ $ HH.ClassName "pure-menu restricted-width"]
       [ HH.ul [ HP.class_ $ HH.ClassName "pure-menu-list intervals-chart"]
         (renderInterval <$> aIntervals)
       ]

    mkButton :: String -> String -> (Unit -> Query Unit) -> H.ParentHTML Query CG.Query Slot Aff
    mkButton text class_ query =
      HH.div_
      [ HH.button [ HP.class_ $ HH.ClassName ("pure-button color-button button-" <> class_)
                  , HE.onClick $ HE.input_ query
                  , HP.type_ $ HP.ButtonButton
                  ]
        [ HH.text text ]
      ]

    mkOption :: forall p i. Set String -> String -> HH.HTML p i
    mkOption enabled k =
      HH.option [ HP.value k
                , HP.enabled (S.member k enabled)
                ] [ HH.text k ]

    mkSelect :: forall v. String ->
                Map String v ->
                Set String ->
                String ->
                (String -> Unit -> Query Unit) ->
                H.ParentHTML Query CG.Query Slot Aff
    mkSelect label items enabled value query =
      let
        keys = A.fromFoldable $ M.keys items
        defaultOption = mkOption S.empty defaultOptionValue
      in
       HH.div [ HP.class_ $ HH.ClassName "gct-select-div"
              , HP.ref $ H.RefLabel label
              ]
       [ HH.label [HP.for label] [ HH.text $ label <> ": " ]
       , HH.select ([ HP.name label
                    , HP.value $ if S.member value enabled then value else defaultOptionValue
                    , HE.onValueChange (HE.input query)
                    ]
)         (defaultOption : (mkOption enabled <$> keys))
       ]

guitarMap :: Map String Guitar
guitarMap = T.taggedToMap G.allGuitars

chordMap :: Map String Chord
chordMap = T.taggedToMap C.allChords

noteMap :: Map String Note
noteMap = T.taggedToMap N.allNotes

allGenChords :: Map String (Map String ThisChord)
allGenChords =
   let
     forNote :: T.Tagged Note -> Tuple String (Map String ThisChord)
     forNote note =
       let
         chords :: List (T.Tagged Chord)
         chords = L.fromFoldable C.allChords
       in
        Tuple (T.getName note) (M.fromFoldable $ (\c -> Tuple (T.getName c) $ C.generateChord (T.getValue c)(T.getValue note)) <$> chords)

     mapping :: List (Tuple String (Map String ThisChord))
     mapping =
       let
         notes :: List (T.Tagged Note)
         notes = L.fromFoldable N.allNotes
       in
        forNote <$> notes
   in
    M.fromFoldable mapping

eval :: forall m. Query ~> H.ParentDSL State Query CG.Query Slot Void m
eval = case _ of
  ChangeGuitar name next -> do
    let mguitar = M.lookup name guitarMap
    case M.lookup name guitarMap of
      Nothing -> pure next
      Just guitar -> do
        H.modify_ (_ { currentGuitar = guitar
                     , slot = name
                     , selectedNotes = S.empty :: Set Note
                     })
        _ <- H.query (Slot name) $ H.request CG.ClearAll
        thisChord <- H.gets getFilteredChord
        case thisChord of
          Nothing -> pure next
          Just chord ->  do
            _ <- H.query (Slot name) $ H.request (CG.ShowChord chord)
            showColor <- H.gets (_.showColor)
            _ <- H.query (Slot name) $ H.request (CG.ShowColor showColor)
            pure next
  ChangeChord name next -> do
    case M.lookup name chordMap of
      Nothing -> do
        H.modify_ (_ { currentChord = defaultOptionValue
                     , filteredIntervals = emptyFilter
                     })
        pure next
      Just newChord -> do
        H.modify_ (_ { currentChord = name
                     , filteredIntervals = emptyFilter
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
      Nothing -> do
        H.modify_ (_ { currentNote = defaultOptionValue
                     , filteredIntervals = emptyFilter
                     })
        pure next
      Just newNote -> do
        oldNote <- H.gets getNote
        filteredIntervals <- H.gets (_.filteredIntervals)
        let change = (flip N.noteDistance) newNote <$> oldNote
            newFilter =
              maybe emptyFilter (\d -> S.map ((flip N.incNoteBy) d) filteredIntervals) change
        H.modify_ (_ { currentNote = name
                     , filteredIntervals = newFilter
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
  ToggleInterval interval next -> do
    filteredIntervals <- H.gets (_.filteredIntervals)
    let newFilter = if S.member interval filteredIntervals
                    then S.delete interval filteredIntervals
                    else S.insert interval filteredIntervals
    H.modify_ (_ { filteredIntervals = newFilter })
    thisChord <- H.gets getFilteredChord
    case thisChord of
      Just chord -> do
        slot <- H.gets (_.slot)
        _ <- H.query (Slot slot) $ H.request (CG.ShowChord chord)
        pure next
      Nothing -> pure next
  ClearSelected next -> do
    slot <- H.gets (_.slot)
    guitar <- H.gets (_.currentGuitar)
    _ <- H.query (Slot slot) $ H.request CG.ClearToggled
    H.modify_ ( _ { selectedNotes = S.empty :: Set Note })
    pure next
  ClearAll next -> do
    slot <- H.gets (_.slot)
    guitar <- H.gets (_.currentGuitar)
    _ <- H.query (Slot slot) $ H.request CG.ClearAll
    H.put initialState
    H.modify_ ( _ { currentGuitar = guitar
                  , slot = slot
                  })
    pure next
  HandleGuitar (CG.Selected notes) next -> do
    H.modify_ (_ { selectedNotes = notes })
    pure next
