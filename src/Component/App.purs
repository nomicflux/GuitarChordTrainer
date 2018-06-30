module Component.App where

import Prelude

import Chord (Chord, IntervalledNote, ThisChord)
import Chord as C
import Chord as I
import Component.Constants (guitarCookie, pushedFretRadius)
import Component.Cookie (setCookie)
import Component.FontAwesome (icon)
import Component.FretColor (fretColor)
import Component.Guitar as CG
import Component.SVG as SVG
import Control.Alt ((<|>))
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
             , showSidebar :: Boolean
             }

defaultOptionValue :: String
defaultOptionValue = ""

emptyFilter :: Set Note
emptyFilter = S.empty

initialState :: Maybe String -> State
initialState cookieGuitar =
  let
    mcookie = do
      name <- cookieGuitar
      guitar <- M.lookup name guitarMap
      pure $ T.Tagged name guitar
    mguitar = mcookie <|> G.allGuitars !! 0
  in
   { currentGuitar: maybe G.standardGuitar T.getValue mguitar
   , currentChord: defaultOptionValue
   , currentNote: defaultOptionValue
   , slot: maybe defaultOptionValue T.getName mguitar
   , showColor: true
   , filteredIntervals: emptyFilter
   , selectedNotes: emptyFilter
   , showSidebar: false
   }

getNote :: State -> Maybe Note
getNote state = M.lookup state.currentNote noteMap

getChord :: State -> Maybe ThisChord
getChord state =
  M.lookup state.currentChord allGenChords >>= M.lookup state.currentNote

getFilteredChord :: State -> Maybe ThisChord
getFilteredChord state =
  (flip C.filterNotes) state.filteredIntervals <$> getChord state

data Query a = ChangeGuitar String a
             | ChangeChord String a
             | ChangeNote String a
             | ToggleShowColor a
             | ToggleSidebar a
             | ToggleInterval Note a
             | ClearSelected a
             | ClearAll a
             | HandleGuitar CG.Message a

data Slot = Slot String
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: H.Component HH.HTML Query (Maybe String) Void Aff
component =
  H.parentComponent
  { initialState: initialState
  , render
  , eval
  , receiver: const Nothing
  }

isFiltered :: State -> Note -> Boolean
isFiltered state note = S.member note state.filteredIntervals

isFit :: String -> String -> Set Note -> Boolean
isFit note chord allowed =
  let
    thisChord = M.lookup chord allGenChords >>= M.lookup note
  in
   maybe false (\c -> S.intersection allowed c.chord == allowed) thisChord

filteredNotes :: String -> Set Note -> Set String
filteredNotes currentChord allowedNotes =
  if S.isEmpty allowedNotes then M.keys noteMap
  else
    S.fromFoldable $ L.filter (\k -> isFit k currentChord allowedNotes) $ L.fromFoldable $ M.keys noteMap

filteredChords :: Set Note -> Set String
filteredChords allowedNotes =
  if S.isEmpty allowedNotes then M.keys chordMap
  else
    let
      allNotes = L.fromFoldable $ M.keys noteMap
      theseNotes c = L.fromFoldable $ filteredNotes c allowedNotes
      chordNotes :: String -> List ThisChord
      chordNotes c =
        L.mapMaybe (\n -> M.lookup c allGenChords >>= M.lookup n) (theseNotes c)
      hasNotes :: String -> Boolean
      hasNotes n = (not L.null) (chordNotes n)
    in
     S.fromFoldable $ L.filter hasNotes $ L.fromFoldable $ M.keys chordMap

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
  ,  HH.div [ HP.class_ $ HH.ClassName "pure-u-1 pure-u-md-1-2 pure-u-lg-2-3 guitar-container" ]
     [ HH.slot (Slot state.slot) CG.component state.currentGuitar $ HE.input HandleGuitar ]
  ]
  where
    renderSidebarToggle :: H.ParentHTML Query CG.Query Slot Aff
    renderSidebarToggle =
      HH.div [ HE.onClick $ HE.input_ ToggleSidebar
             , HP.class_ $ HH.ClassName ("sidebar-toggle phone-only " <> if state.showSidebar then "sidebar-shown" else "sidebar-hidden")
             ]
      [ icon "ellipsis-h" ]

    renderSelects :: H.ParentHTML Query CG.Query Slot Aff
    renderSelects =
      HH.div [ HP.class_ $ HH.ClassName "select-div" ]
      [ mkSelect tuningRefName guitarMap (M.keys guitarMap) state.slot ChangeGuitar
      , mkSelect chordRefName chordMap (filteredChords state.selectedNotes) state.currentChord ChangeChord
      , mkSelect noteRefName noteMap (filteredNotes state.currentChord state.selectedNotes) state.currentNote ChangeNote
      ]

    renderButtons :: H.ParentHTML Query CG.Query Slot Aff
    renderButtons =
      HH.div [ HP.class_ $ HH.ClassName "button-div" ]
      [ mkButton ((if state.showColor then "Hide" else "Show") <> " Interval Colors") (if state.showColor then "toggle-on" else "toggle-off") "plain" ToggleShowColor
      , mkButton "Clear Selected Frets" "eraser" "warning" ClearSelected
      , mkButton "Clear All" "undo" "error" ClearAll
      ]

    renderSidebarContent :: H.ParentHTML Query CG.Query Slot Aff
    renderSidebarContent =
      HH.div [ HP.class_ $ HH.ClassName ("sidebar-content " <> if state.showSidebar then "sidebar-shown" else "sidebar-hidden") ]
      [ HH.form [ HP.class_ $ HH.ClassName "pure-form" ] $
        [ renderSelects
        , renderButtons
        ] <> maybe [] (A.singleton <<< renderIntervalChart <<< C.chordToIntervals) (getChord state)
      ]

    renderSidebar :: H.ParentHTML Query CG.Query Slot Aff
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName ("pure-u-1 pure-u-sm-1 pure-u-md-1-2 pure-u-lg-1-3 sidebar " <> if state.showSidebar then "sidebar-shown" else "sidebar-hidden") ]
      [ renderSidebarToggle
      , renderSidebarContent
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

    mkButton :: String -> String -> String -> (Unit -> Query Unit) -> H.ParentHTML Query CG.Query Slot Aff
    mkButton screenText phoneIcon class_ query =
      HH.div [ HP.class_ $ HH.ClassName "button-div" ]
      [ HH.button [ HP.class_ $ HH.ClassName ("pure-button color-button button-" <> class_)
                  , HE.onClick $ HE.input_ query
                  , HP.type_ $ HP.ButtonButton
                  ]
        [ HH.span [ HP.class_ $ HH.ClassName "button-icon" ] [ icon phoneIcon ]
        , HH.span [ HP.class_ $ HH.ClassName "screen-only" ] [ HH.text $ " " <> screenText ]
        ]
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
              -- , HP.ref $ H.RefLabel label
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
     forChord :: T.Tagged Chord -> Tuple String (Map String ThisChord)
     forChord chord =
       let
         notes :: List (T.Tagged Note)
         notes = L.fromFoldable N.allNotes
       in
        Tuple (T.getName chord) (M.fromFoldable $ (\n -> Tuple (T.getName n) $ C.generateChord (T.getValue chord)(T.getValue n)) <$> notes)

     mapping :: List (Tuple String (Map String ThisChord))
     mapping =
       let
         chords :: List (T.Tagged Chord)
         chords = L.fromFoldable C.allChords
       in
        forChord <$> chords
   in
    M.fromFoldable mapping

eval :: Query ~> H.ParentDSL State Query CG.Query Slot Void Aff
eval (ChangeGuitar name next) = do
  let mguitar = M.lookup name guitarMap
  case M.lookup name guitarMap of
    Nothing -> pure next
    Just guitar -> do
      _ <- H.liftEffect $ setCookie guitarCookie name
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
eval (ChangeChord name next) = do
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
eval (ChangeNote name next) = do
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
eval (ToggleShowColor next) = do
  state <- H.get
  let nowShow = not state.showColor
  H.modify_ (_ { showColor = nowShow })
  _ <- H.query (Slot state.slot) $ H.request (CG.ShowColor nowShow)
  pure next
eval (ToggleInterval interval next) = do
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
eval (ToggleSidebar next) = do
  showSidebar <- H.gets (_.showSidebar)
  H.modify_ (_ { showSidebar = not showSidebar })
  pure next
eval (ClearSelected next) = do
  state <- H.get
  _ <- H.query (Slot state.slot) $ H.request CG.ClearToggled
  H.modify_ ( _ { selectedNotes = S.empty :: Set Note })
  pure next
eval (ClearAll next) = do
  slot <- H.gets (_.slot)
  _ <- H.query (Slot slot) $ H.request CG.ClearAll
  H.modify_ ( _ { currentChord = defaultOptionValue
                , currentNote = defaultOptionValue
                , selectedNotes = emptyFilter
                , filteredIntervals = emptyFilter
                })
  pure next
eval (HandleGuitar (CG.Selected notes) next) = do
  H.modify_ (_ { selectedNotes = notes })
  pure next
