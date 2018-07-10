module Component.App where

import Prelude

import Chord (Chord)
import Chord as C
import Component.Common.Constants (guitarCookie, pushedFretRadius, scaleChordCookie)
import Component.Common.Cookie (setCookie)
import Component.Common.FontAwesome (icon)
import Component.Common.FretColor (fretColor)
import Component.Common.RootedInterval (IntervalledNote, RootedInterval, filterNotes, fromChord, fromScale, toIntervals)
import Component.Common.RootedInterval as RI
import Component.Common.SVG as SVG
import Component.Guitar as CG
import Control.Alt ((<|>))
import Data.Array ((!!), (:))
import Data.Array as A
import Data.List (List)
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
import Scale (Scale)
import Scale as Sc
import Tagged as T

data Using = Scales | Chords

flipUsing :: Using -> Using
flipUsing Scales = Chords
flipUsing Chords = Scales

type State = { currentChord :: String
             , currentScale :: String
             , currentChordNote :: String
             , currentScaleNote :: String
             , showColor :: Boolean
             , filteredIntervals :: Set Note
             , selectedNotes :: Set Note
             , showSidebar :: Boolean
             , scalesOrChords :: Using
             , snapshotId :: Int
             , snapshotGuitars :: Array String
             }

defaultOptionValue :: String
defaultOptionValue = ""

emptyFilter :: Set Note
emptyFilter = S.empty

type Input = { guitar :: Maybe String
             , scalesOrChords :: Maybe String
             }

scaleChordFromString :: String -> Maybe Using
scaleChordFromString "scale" = Just Scales
scaleChordFromString "chord" = Just Chords
scaleChordFromString _ = Nothing

scaleChordToString :: Using -> String
scaleChordToString Scales = "scale"
scaleChordToString Chords = "chord"

initialState :: Input -> State
initialState input =
  let
    mguitar = input.guitar <|> (T.getName <$> G.allGuitars !! 0)
    guitar = fromMaybe defaultOptionValue mguitar
  in
   { currentChord: defaultOptionValue
   , currentScale: defaultOptionValue
   , currentChordNote: defaultOptionValue
   , currentScaleNote: defaultOptionValue
   , showColor: true
   , filteredIntervals: emptyFilter
   , selectedNotes: emptyFilter
   , showSidebar: false
   , scalesOrChords: fromMaybe Chords $ input.scalesOrChords >>= scaleChordFromString
   , snapshotId: 0
   , snapshotGuitars: [guitar]
   }

getNote :: State -> Maybe Note
getNote state =
  case state.scalesOrChords of
    Scales -> M.lookup state.currentScaleNote noteMap
    Chords -> M.lookup state.currentChordNote noteMap

getChord :: State -> Maybe RootedInterval
getChord state =
  M.lookup state.currentChord allGenChords >>= M.lookup state.currentChordNote

getScale :: State -> Maybe RootedInterval
getScale state =
  M.lookup state.currentScale allGenScales >>= M.lookup state.currentScaleNote

getFilteredChord :: State -> Maybe RootedInterval
getFilteredChord state =
  (flip filterNotes) state.filteredIntervals <$> getChord state

getFilteredScale :: State -> Maybe RootedInterval
getFilteredScale state =
  (flip filterNotes) state.filteredIntervals <$> getScale state

getFiltered :: State -> Maybe RootedInterval
getFiltered state =
  case state.scalesOrChords of
    Scales -> getFilteredScale state
    Chords -> getFilteredChord state

data Query a = ChangeGuitar String a
             | ChangeChord String a
             | ChangeScale String a
             | ChangeNote String a
             | ToggleShowColor a
             | ToggleSidebar a
             | ToggleInterval Note a
             | ToggleScalesChords a
             | ClearSelected a
             | ClearAll a
             | TakeSnapshot a
             | HandleGuitar CG.Message a

data Slot = Slot Int String
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: H.Component HH.HTML Query Input Void Aff
component =
  H.parentComponent
  { initialState: initialState
  , render
  , eval
  , receiver: const Nothing
  }

isFiltered :: State -> Note -> Boolean
isFiltered state note = S.member note state.filteredIntervals

isFit :: Map String (Map String RootedInterval) ->
         String -> String -> Set Note -> Boolean
isFit intervalMap note interval allowed =
  let
    theseIntervals = M.lookup interval intervalMap >>= M.lookup note
  in
   maybe false (\c -> S.intersection allowed c.notes == allowed) theseIntervals

filteredNotes :: Map String (Map String RootedInterval) ->
                 String -> Set Note -> Set String
filteredNotes intervalMap currentIntervals allowedNotes =
  if S.isEmpty allowedNotes then M.keys noteMap
  else
    S.fromFoldable $ L.filter (\k -> isFit intervalMap k currentIntervals allowedNotes) $ L.fromFoldable $ M.keys noteMap

filteredIntervals :: Map String (Map String RootedInterval) ->
                     Set String ->
                     Set Note ->
                     Set String
filteredIntervals intervalMap def allowedNotes =
  if S.isEmpty allowedNotes then def
  else
    let
      allNotes = L.fromFoldable $ M.keys noteMap
      theseNotes :: String -> List String
      theseNotes i = L.fromFoldable $ filteredNotes intervalMap i allowedNotes
      intervalNotes i =
        L.mapMaybe (\n -> M.lookup i intervalMap >>= M.lookup n) (theseNotes i)
      hasNotes :: String -> Boolean
      hasNotes n = (not L.null) (intervalNotes n)
    in
     S.fromFoldable $ L.filter hasNotes $ L.fromFoldable $ def

filteredChords :: Set Note -> Set String
filteredChords =
  filteredIntervals allGenChords (M.keys chordMap)

filteredScales :: Set Note -> Set String
filteredScales =
  filteredIntervals allGenScales (M.keys scaleMap)

tuningRefName :: String
tuningRefName = "Tuning"

chordRefName :: String
chordRefName = "Chord"

scaleRefName :: String
scaleRefName = "Scale"

noteRefName :: String
noteRefName = "Note"

getCurrentGuitar :: State -> String
getCurrentGuitar state =
  fromMaybe defaultOptionValue (state.snapshotGuitars !! state.snapshotId)

render :: State -> H.ParentHTML Query CG.Query Slot Aff
render state =
  HH.div [ HP.class_ $ HH.ClassName "pure-g" ]
  [ renderSidebar
  , HH.div [ HP.class_ $ HH.ClassName "pure-u-1 pure-u-md-2-3 pure-u-lg-3-4 guitar-container"
            ] $ A.concatMap renderGuitar (A.range state.snapshotId 0)
  ]
  where
    renderGuitar :: Int -> Array (H.ParentHTML Query CG.Query Slot Aff)
    renderGuitar snapshot =
      let
        atSnapshot = fromMaybe defaultOptionValue (state.snapshotGuitars !! snapshot)
        mguitar = M.lookup atSnapshot guitarMap
      in maybe [] (\g -> [ HH.slot (Slot snapshot atSnapshot) CG.component g (HE.input HandleGuitar) ]) mguitar

    renderSidebarToggle :: H.ParentHTML Query CG.Query Slot Aff
    renderSidebarToggle =
      HH.div [ HE.onClick $ HE.input_ ToggleSidebar
             , HP.class_ $ HH.ClassName ("sidebar-toggle phone-only " <> if state.showSidebar then "sidebar-shown" else "sidebar-hidden")
             ]
      [ icon "ellipsis-h" ]

    renderSelects :: H.ParentHTML Query CG.Query Slot Aff
    renderSelects =
      let noteGetter =
            case state.scalesOrChords of
              Scales -> allGenScales
              Chords -> allGenChords
          noteSource =
            case state.scalesOrChords of
              Scales -> state.currentScale
              Chords -> state.currentChord
          currentNote =
            case state.scalesOrChords of
              Scales -> state.currentScaleNote
              Chords -> state.currentChordNote
      in
       HH.div [ HP.class_ $ HH.ClassName "select-div" ]
       [ mkSelect tuningRefName guitarMap (M.keys guitarMap) (getCurrentGuitar state) ChangeGuitar
       , case state.scalesOrChords of
         Scales ->
           mkSelect scaleRefName scaleMap (filteredScales state.selectedNotes) state.currentScale ChangeScale
         Chords ->
           mkSelect chordRefName chordMap (filteredChords state.selectedNotes) state.currentChord ChangeChord
       , mkSelect noteRefName noteMap (filteredNotes noteGetter noteSource state.selectedNotes) currentNote ChangeNote
       ]

    renderButtons :: H.ParentHTML Query CG.Query Slot Aff
    renderButtons =
      let
        colorText = if state.showColor then "Hide" else "Show"
        colorIcon = if state.showColor then "toggle-on" else "toggle-off"
        scaleChordText =
          case state.scalesOrChords of
            Scales -> "Chords"
            Chords -> "Scales"
        scaleChordIcon =
          case state.scalesOrChords of
            Scales -> "ruler-horizontal"
            Chords -> "ruler-vertical"
      in
       HH.div [ HP.class_ $ HH.ClassName "button-div" ]
       [ mkButton (colorText <> " Interval Colors") colorIcon "plain" ToggleShowColor
       , mkButton ("Show " <> scaleChordText) scaleChordIcon "secondary" ToggleScalesChords
       , mkButton "Clear Selected Frets" "eraser" "warning" ClearSelected
       , mkButton "Clear All" "undo" "error" ClearAll
       , mkButton "Snapshot (Experimental)" "camera" "primary screen-only" TakeSnapshot
       ]

    renderSidebarContent :: H.ParentHTML Query CG.Query Slot Aff
    renderSidebarContent =
      let intervals =
            case state.scalesOrChords of
              Scales -> getScale state
              Chords -> getChord state
      in
       HH.div [ HP.class_ $ HH.ClassName ("sidebar-content " <> if state.showSidebar then "sidebar-shown" else "sidebar-hidden") ]
       [ HH.form [ HP.class_ $ HH.ClassName "pure-form" ] $
         [ renderSelects
         , renderButtons
         ] <> maybe [] (A.singleton <<< renderIntervalChart <<< toIntervals) intervals
       ]

    renderSidebar :: H.ParentHTML Query CG.Query Slot Aff
    renderSidebar =
      HH.div [ HP.class_ $ HH.ClassName ("pure-u-1 pure-u-sm-1 pure-u-md-1-3 pure-u-lg-1-4 sidebar " <> if state.showSidebar then "sidebar-shown" else "sidebar-hidden") ]
      [ renderSidebarToggle
      , renderSidebarContent
      ]

    renderInterval :: IntervalledNote ->
                      H.ParentHTML Query CG.Query Slot Aff
    renderInterval interval =
      let halfWidth = pushedFretRadius + 1
          halfHeight = pushedFretRadius + 1
          class_ = if isFiltered state (RI.getNote interval) then "filtered" else "unfiltered"
          filterInterval x = state.showColor || (RI.getInterval x) == 0
          f i = if filterInterval i
                then fretColor (RI.getInterval i)
                else "black"
      in
       HH.li
       [ HP.class_ $ HH.ClassName ("pure-menu-item restricted-height interval-key " <> class_)
       , HE.onClick $ HE.input_ (ToggleInterval (RI.getNote interval))
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
       , HH.text $ " = " <> intervalToName (RI.getInterval interval)
       ]

    renderIntervalChart :: Set IntervalledNote ->
                           H.ParentHTML Query CG.Query Slot Aff
    renderIntervalChart intervals =
      let
        aIntervals = A.fromFoldable intervals
        scClass = case state.scalesOrChords of
          Scales -> "on-scales"
          Chords -> "on-chords"
      in
       HH.div [ HP.class_ $ HH.ClassName ("pure-menu restricted-width " <> scClass)]
       [ HH.ul [ HP.class_ $ HH.ClassName ("pure-menu-list intervals-chart " <> scClass)]
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

scaleMap :: Map String Scale
scaleMap = T.taggedToMap Sc.allScales

noteMap :: Map String Note
noteMap = T.taggedToMap N.allNotes

genAll :: forall a b.
          (b -> RootedInterval) ->
          (a -> Note -> b) ->
          Array (T.Tagged a) ->
          Map String (Map String RootedInterval)
genAll transformer generator items =
   let
     forRI :: T.Tagged a -> Tuple String (Map String RootedInterval)
     forRI ri =
       let
         notes :: List (T.Tagged Note)
         notes = L.fromFoldable N.allNotes
       in
        Tuple (T.getName ri) (M.fromFoldable $ (\n -> Tuple (T.getName n) $ transformer $ generator (T.getValue ri)(T.getValue n)) <$> notes)

     mapping :: List (Tuple String (Map String RootedInterval))
     mapping =
       let
         intervals :: List (T.Tagged a)
         intervals = L.fromFoldable items
       in
        forRI <$> intervals
   in
    M.fromFoldable mapping


allGenChords :: Map String (Map String RootedInterval)
allGenChords = genAll fromChord C.generateChord C.allChords

allGenScales :: Map String (Map String RootedInterval)
allGenScales = genAll fromScale Sc.generateScale Sc.allScales

getSlot :: State -> Slot
getSlot state =
  let mname = state.snapshotGuitars !! state.snapshotId
  in Slot state.snapshotId (fromMaybe defaultOptionValue mname)

clearAndResend :: forall m a. a -> H.ParentDSL State Query CG.Query Slot Void m a
clearAndResend next = do
  slot <- H.gets getSlot
  _ <- H.query slot $ H.request CG.ClearAll
  filtered <- H.gets getFiltered
  case filtered of
    Nothing -> pure next
    Just intervals ->  do
      _ <- H.query slot $ H.request (CG.ShowNotes intervals)
      showColor <- H.gets (_.showColor)
      _ <- H.query slot $ H.request (CG.ShowColor showColor)
      pure next

eval :: Query ~> H.ParentDSL State Query CG.Query Slot Void Aff
eval (ChangeGuitar name next) = do
  let mguitar = M.lookup name guitarMap
  case M.lookup name guitarMap of
    Nothing -> pure next
    Just guitar -> do
      _ <- H.liftEffect $ setCookie guitarCookie name
      guitars <- H.gets (_.snapshotGuitars)
      snapshotId <- H.gets (_.snapshotId)
      H.modify_ (_ { selectedNotes = S.empty :: Set Note
                   , snapshotGuitars = fromMaybe [] $ A.updateAt snapshotId name guitars
                   })
      clearAndResend next
eval (ChangeChord name next) = do
  case M.lookup name chordMap of
    Nothing -> pure next
    Just newChord -> do
      H.modify_ (_ { currentChord = name
                   , filteredIntervals = emptyFilter
                   })
      thisChord <- H.gets getChord
      case thisChord of
        Nothing -> pure next
        Just chord -> do
          slot <- H.gets getSlot
          _ <- H.query slot $ H.request (CG.ShowNotes chord)
          pure next
eval (ChangeScale name next) = do
  case M.lookup name scaleMap of
    Nothing -> pure next
    Just newScale -> do
      H.modify_ (_ { currentScale = name
                   , filteredIntervals = emptyFilter
                   })
      thisScale <- H.gets getScale
      case thisScale of
        Nothing -> pure next
        Just scale -> do
          slot <- H.gets getSlot
          _ <- H.query slot $ H.request (CG.ShowNotes scale)
          pure next
eval (ChangeNote name next) = do
  case M.lookup name noteMap of
    Nothing -> pure next
    Just newNote -> do
      oldNote <- H.gets getNote
      state <- H.get
      let change = (flip N.noteDistance) newNote <$> oldNote
          newFilter =
            maybe emptyFilter (\d -> S.map ((flip N.incNoteBy) d) state.filteredIntervals) change
      case state.scalesOrChords of
        Scales ->
          H.modify_ (_ { currentScaleNote = name, filteredIntervals = newFilter })
        Chords ->
          H.modify_ (_ { currentChordNote = name, filteredIntervals = newFilter })
      filtered <- H.gets getFiltered
      case filtered of
        Nothing -> pure next
        Just theseIntervals -> do
          _ <- H.query (getSlot state) $ H.request (CG.ShowNotes theseIntervals)
          pure next
eval (ToggleShowColor next) = do
  state <- H.get
  let nowShow = not state.showColor
  H.modify_ (_ { showColor = nowShow })
  _ <- H.query (getSlot state) $ H.request (CG.ShowColor nowShow)
  pure next
eval (ToggleInterval interval next) = do
  oldFilter <- H.gets (_.filteredIntervals)
  let newFilter = if S.member interval oldFilter
                  then S.delete interval oldFilter
                  else S.insert interval oldFilter
  H.modify_ (_ { filteredIntervals = newFilter })
  filtered <- H.gets getFiltered
  case filtered of
    Nothing -> pure next
    Just intervals -> do
      slot <- H.gets getSlot
      _ <- H.query slot $ H.request (CG.ShowNotes intervals)
      pure next
eval (ToggleSidebar next) = do
  showSidebar <- H.gets (_.showSidebar)
  H.modify_ (_ { showSidebar = not showSidebar })
  pure next
eval (ToggleScalesChords next) = do
  scalesChords <- H.gets (_.scalesOrChords)
  let newSC = flipUsing scalesChords
  H.modify_ (_ { scalesOrChords = newSC
               , filteredIntervals = emptyFilter
               })
  _ <- H.liftEffect $ setCookie scaleChordCookie (scaleChordToString newSC)
  filtered <- H.gets getFiltered
  case filtered of
    Nothing -> pure next
    Just theseIntervals -> do
      slot <- H.gets getSlot
      _ <- H.query slot $ H.request (CG.ShowNotes theseIntervals)
      pure next
eval (ClearSelected next) = do
  state <- H.get
  _ <- H.query (getSlot state) $ H.request CG.ClearToggled
  H.modify_ ( _ { selectedNotes = S.empty :: Set Note })
  pure next
eval (ClearAll next) = do
  slot <- H.gets getSlot
  guitar <- H.gets getCurrentGuitar
  _ <- H.query slot $ H.request CG.ClearAll
  H.modify_ ( _ { currentChord = defaultOptionValue
                , currentScale = defaultOptionValue
                , currentChordNote = defaultOptionValue
                , currentScaleNote = defaultOptionValue
                , selectedNotes = emptyFilter
                , filteredIntervals = emptyFilter
                , snapshotId = 0
                , snapshotGuitars = [guitar]
                })
  pure next
eval (TakeSnapshot next) = do
  state <- H.get
  let current = getCurrentGuitar state
  H.modify_ (_ { snapshotId = state.snapshotId + 1
               , snapshotGuitars = A.snoc state.snapshotGuitars current
               , selectedNotes = emptyFilter
               })
  clearAndResend next
eval (HandleGuitar (CG.Selected notes) next) = do
  H.modify_ (_ { selectedNotes = notes })
  pure next
