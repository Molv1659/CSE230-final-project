{-# LANGUAGE TemplateHaskell #-}

module UI where

import Prelude as P
import Brick 
    (Widget, BrickEvent, EventM
    , Edges(..)
    ,str
    ,halt, Next
    ,(<+>),(<=>)
    )
import Brick.Main
    (resizeOrQuit
    ,getVtyHandle
    ,continue
    ,halt
    )
import Brick.Widgets.Edit
    (Editor
    ,renderEditor
    ,handleEditorEvent
    ,editor
    ,getEditContents
    )
import Brick.Focus
    (FocusRing
    ,withFocusRing
    ,focusGetCurrent
    ,focusRing
    ,focusRingCursor
    )
import Brick.Widgets.Center
    (hCenterWith
    ,centerLayer
    ,vCenterWith
    )
import Brick.Widgets.Core 
    (hBox, vBox
    ,hLimit, vLimit
    ,hLimitPercent
    ,withBorderStyle, joinBorders
    ,clickable
    ,strWrap
    ,emptyWidget
    ,padRight
    ,padAll
    ,padTop
    ,withBorderStyle
    )
import Brick.Widgets.Border
    (vBorder, hBorder, border, joinableBorder, borderWithLabel)
import Brick.Widgets.Border.Style
    (unicode, unicodeRounded, unicodeBold)
import qualified Brick.Types as T
import Lens.Micro.TH (
    makeLenses
    )
import Lens.Micro (
    over
    , (^.), (&), (.~), (%~)
    )
import Graphics.Vty (
    outputIface
    , Event(..)
    )
import Graphics.Vty.Input.Events (
    Key(..),
    Button(..)
    )
import Graphics.Vty.Output.Interface (
    Output(..),
    Mode(..)
    )
import Lens.Micro.Extras (
    view
    )
import qualified Data.Map.Strict as M
import Control.Monad (
    when
    )
import Control.Monad.IO.Class (
    liftIO
    )
import GoLibrary as Lib
import qualified Network.Socket as S
import qualified Data.List as L
import qualified Brick.Widgets.List as BL

data Tick = Tick

data ResourceName = Board 
    | IPField 
    | ConnectButton 
    | ListenButton
    | PassButton
    | ResignButton
    | OtherResources 
    deriving (Show, Ord, Eq)


data PointPattern = BlackStone | WhiteStone | EmptyStone | HandicapPoint deriving (Eq)
data PointLoc = TopLeft | TopMid | TopRight | RightMid | BottomRight | BottomMid | BottomLeft | LeftMid | Other

-- Take User Input
-- data FormFields = IPField deriving (Show, Ord, Eq)
-- data UserIP = UserIP { _ip :: T.Text } deriving (Show)
-- generate lens to create corresponding fields
-- makeLenses ''UserIP

stoneToPointPattern :: Stone -> PointPattern
stoneToPointPattern Black   = BlackStone
stoneToPointPattern White   = WhiteStone
stoneToPointPattern Ko      = EmptyStone
stoneToPointPattern Empty   = EmptyStone

-- The data structure owned by the UI part
-- Prefix the fields in GameState with underscore "_" to make lenses for it
data GameState = GameState { 
    _dim::Int, 
    _boardState :: Lib.Game, 
    _time::Int, 
    _pointLocMap:: M.Map Lib.Point PointLoc,
    _lastReportedClick :: Maybe (Int, Int),
    _opponentIP :: Editor String ResourceName,
    _editFocus :: FocusRing ResourceName,
    _submitIP :: Bool,
    _notification :: String,
    _myIP :: String,
    _currentRound :: Int,  -- the round used for display,
    _totalRound :: Int,  -- total number of rounds
    _snapshots :: [M.Map Lib.Point PointPattern]
    }

-- make "lenses" for the UI state for maintainable and extendible getter/setter
-- makeLenses ''Lib.Game
makeLenses ''GameState

-- Now to access and update the corresponding fields of a GameState value, do get and set:

-- For getter:
-- Method 1: Use the `^.` in Lens.Micro to get the `_dim` field in state s:
--      s ^. dim
-- Method 2: Use the `view` in Lens.Micro.Extra for getter, e.g. to get _dim of GameState s:
--      view (dim . s)

-- For setter:
-- Method 1: Use the `.~` (or synonym `set`) in Lens.Micro to set the `_dim` field to d in state s :
--      (dim .~ d) s
--      

-- For modifier
-- Method 1: use the `%~` to modify inplace, e.g. to add 1 to the dimension of _dim field of state s:
--      s & dim %~ (+1)

-- Method 2: Use the `over` for setter, e.g. to set _dim of GameState s to 19
--      over dim (\_ -> 19) s


-- pointLocMap is used to decide how to render empty intersections on different parts of the board
-- board index[1..l][1..l]
buildPointLocMap :: Int -> M.Map Point PointLoc
buildPointLocMap l = M.unions [
    M.fromList [((Point 1 1), TopLeft)]
    ,M.fromList [((Point 1 j), TopMid) | 
        j <- [2..(l-1)]]
    ,M.fromList [((Point 1 l), TopRight)]
    ,M.fromList [((Point i l), RightMid) |
        i <- [2..(l-1)]]
    ,M.fromList [((Point l l), BottomRight)]
    ,M.fromList [((Point l j), BottomMid) |
        j <- [2..(l-1)]]
    ,M.fromList [((Point l 1), BottomLeft)]
    ,M.fromList [((Point i 1), LeftMid) |
        i <- [2..(l-1)]]
    ,M.fromList [((Point i j), Other) |
        i <- [2..(l-1)],
        j <- [2..(l-1)]
    ]
    ]

defaultBoardSize :: Int
defaultBoardSize = 19

-- default state before appStartEvent executes
getInitialState :: GameState
getInitialState = 
    let d = defaultBoardSize
        stone = Black
        points = [((Point x y), EmptyStone) | x <- [1..d], y <- [1..d]]
        s = GameState {
        _dim=d,  -- size of the board, need to be updated in appStartEvent
        _boardState = createGo stone d,
        _time=0,   -- how many rounds have passed
        _pointLocMap=buildPointLocMap d,  -- locations of black
        _lastReportedClick=Nothing,
        _myIP="Placeholder",
        _opponentIP=(editor IPField Nothing ""),
        _editFocus=(focusRing [IPField]),
        _submitIP=False,
        _notification="Mock Notification",
        _currentRound=0,
        _totalRound=0,
        _snapshots=[M.fromList points]
        }
    in s

decidePointLoc :: GameState -> Int -> Int -> PointLoc
decidePointLoc g i j = 
    let pl = M.lookup (Point i j) (g ^. pointLocMap)
    in case pl of
        Nothing -> Other
        Just loc -> loc

isStone :: PointPattern -> GameState -> Int -> Int -> Bool
isStone p g i j = case M.lookup (Lib.Point {_i = i, _j = j}) ((g ^. snapshots) !! (g ^. currentRound)) of
    Nothing -> False
    Just stone -> stone == p


isHandiCap :: Int -> Int -> Int -> Bool
isHandiCap l i j =
    case l of 
        19 -> if (i == 4 || i == 10 || i == 16) && (j == 4 || j == 10 || j == 16) then
            True
        else
            False
        13 -> if (i == 7 && j == 7) || ((i == 4 && i == 10) && (j == 4 && j == 10)) then
            True
        else
            False
        9 -> if (i == 5 && j == 5) || ((i == 3 || i == 7) && (j == 3 || j == 7)) then
            True
        else
            False
        _ -> False  -- Only support drawing star point for 9x9 and 13x13 and 19x19

decidePointPattern :: GameState -> Int -> Int -> PointPattern
decidePointPattern g i j = 
    if isStone BlackStone g i j then
        BlackStone
    else if isStone WhiteStone g i j then
        WhiteStone
    else if isHandiCap (decideBoardDim g) i j then
        HandicapPoint
    else
        EmptyStone

decideBoardDim :: GameState -> Int
decideBoardDim g = g ^. dim

-- Game Visualizer
-- the entrance function of drawing
drawUi :: GameState -> [Widget ResourceName]
drawUi g = [
    vBox [
        hBorder,
        hBox [drawLeftColumn g
            ,vBorder
            ,drawBoard g
            ,vBorder
            ,drawGameInfo g
            ]
        ]
    ]

-- display game room name on the top
drawRoomInfo :: GameState -> Widget ResourceName
drawRoomInfo g = vLimit 1 $ hBox [hBorder, vBorder, str " Test room ", vBorder, hBorder]

-- display IP info and connect/listen buttons
drawIPInfo :: GameState -> Widget ResourceName
drawIPInfo g = case g^.submitIP of
    True -> drawMyIP g
            <=> (str "Opponent's IP: " <+> (str . unlines $ getEditContents $ g^.opponentIP)) <=> hBorder
    _    -> vBox [strWrap "Please enter the opponent's IP and click CONNECT or LISTEN to connection request to start a game."
                ,hCenterWith (Just '-') (str "-")
                ,drawMyIP g
                ,drawEditor g
                ,str "\n"
                ,(drawButton ConnectButton "Connect" <+> padAll 1 (str "OR") <+> drawButton ListenButton "Listen")
                ,str "\n"
                ,hBorder]

-- display some notification on the bottom panel
drawNotification :: GameState -> Widget ResourceName
drawNotification g = vBox [strWrap "Notification", strWrap (g^.notification)]

-- display player/watchers information on the left panel
drawLeftColumn :: GameState -> Widget ResourceName
drawLeftColumn g = padAll 1 $ hLimit 30 $ vBox [
                    drawIPInfo g
                    ,drawNotification g]

-- draw the current board and other game info using GameState in the middle panel
-- currently an empty board is drawn using realDrawBoard
drawBoard :: GameState -> Widget ResourceName
drawBoard g = vBox [vLimit 1 $ hBox [hBorder, vBorder, str " Board ", vBorder, hBorder]
                    ,realDrawBoard g
                    ,drawLastClick g
                    ,printRounds g
                    -- ,printMoveResults g
                    ]

printRounds :: GameState -> Widget ResourceName
printRounds g = hCenterWith Nothing (vBox[
    hCenterWith Nothing $ str $ "Round: " ++ show (g ^. currentRound) ++ "/" ++ show (g ^. totalRound)
    , hCenterWith Nothing $ str "Tip: Use Left/Right Key to view history"
    ])

-- After validate move, display hints from logic for invalid move
-- printMoveResults :: GameState -> Widget

-- helper function, either draw a stone/empty intersection point
-- currently only draws empty intersection point
stoneOrEmpty :: PointPattern -> PointLoc -> Widget ResourceName
stoneOrEmpty BlackStone _           = str {- Large Circle -}"\x25EF"
stoneOrEmpty HandicapPoint _        = str {- Square position indicator -}"\x2BD0"
stoneOrEmpty WhiteStone _           = str {- Black Circle for record -}"\x23FA"
stoneOrEmpty EmptyStone TopLeft     = joinableBorder $ Edges {eTop=False, eBottom=True, eLeft=False, eRight=True}
stoneOrEmpty EmptyStone TopMid      = joinableBorder $ Edges {eTop=False, eBottom=True, eLeft=True, eRight=True}
stoneOrEmpty EmptyStone TopRight    = joinableBorder $ Edges {eTop=False, eBottom=True, eLeft=True, eRight=False}
stoneOrEmpty EmptyStone LeftMid     = joinableBorder $ Edges {eTop=True, eBottom=True, eLeft=False, eRight=True}
stoneOrEmpty EmptyStone RightMid    = joinableBorder $ Edges {eTop=True, eBottom=True, eLeft=True, eRight=False}
stoneOrEmpty EmptyStone BottomLeft  = joinableBorder $ Edges {eTop=True, eBottom=False, eLeft=False, eRight=True}
stoneOrEmpty EmptyStone BottomMid   = joinableBorder $ Edges {eTop=True, eBottom=False, eLeft=True, eRight=True}
stoneOrEmpty EmptyStone BottomRight = joinableBorder $ Edges {eTop=True, eBottom=False, eLeft=True, eRight=False}
stoneOrEmpty EmptyStone Other       = joinableBorder $ Edges {eTop=True, eBottom=True, eLeft=True, eRight=True}

-- helper function, either draw a stone/empty intersection point using `stoneOrEmpty`
-- deal with rightmost column as they don't need an extra hborder on their right.
basicGrid :: PointPattern -> PointLoc -> Widget ResourceName
basicGrid s l = case l of
    -- the last column needs special treatment
    TopRight -> stoneOrEmpty s l
    RightMid -> stoneOrEmpty s l
    BottomRight -> stoneOrEmpty s l
    _ -> hBox [stoneOrEmpty s l, h_border]
        where
            h_border = joinableBorder $ Edges {eTop=False, eBottom=False, eLeft=True, eRight=True}
    

-- Literally, do the real job to draw the board
realDrawBoard :: GameState -> Widget ResourceName
realDrawBoard g = 
    let d = decideBoardDim g
    in centerLayer $ clickable Board $ vBox [
            hBox [ 
                (basicGrid (decidePointPattern g i j) (decidePointLoc g i j)) 
                    | j <- [1..d]
                ]
            | i <- [1..d]
        ]

-- Display the last click position under the board
drawLastClick :: GameState -> Widget ResourceName
drawLastClick g = str ("Last Stone was placed at: " ++ (show (g ^. lastReportedClick)))

-- create border box for player info
makeBorderBox :: String -> Stone -> Int -> Bool -> Widget ResourceName
makeBorderBox label stone score isTurn = hCenterWith Nothing $ 
    case isTurn of
        True -> withBorderStyle unicodeBold
        _    -> withBorderStyle unicodeRounded
    $ borderWithLabel (str label) $
    hLimit 20 $
    vLimit 10 $
    (padAll 1 $ padRight T.Max $
        vBox [str "Color: " <+> str (show stone)
            ,str "Score: " <+> str (show score)])

-- display some game stats on the right panel
-- TODO: change black move to dynamic + create timer
drawGameInfo :: GameState -> Widget ResourceName
drawGameInfo g = hLimit 30 $ vBox [padAll 1 $ hCenterWith Nothing $ str "Black move: 10:00"
                     ,case g^.boardState^.player of
                         Black -> makeBorderBox "Myself" Black (g^.boardState^.scoreBlack) True
                                <=> makeBorderBox "Opponent" White (g^.boardState^.scoreWhite) False
                         _     -> makeBorderBox "Myself" White (g^.boardState^.scoreWhite) True
                                <=> makeBorderBox "Opponent" Black (g^.boardState^.scoreBlack) False
                     ,hCenterWith Nothing $ hLimit 10 $ drawButton PassButton "Pass"
                     ,hCenterWith Nothing $ hLimit 10 $ drawButton ResignButton "Resign"
                    ]

inferCoordinate :: T.Location -> Maybe (Int, Int)
inferCoordinate (T.Location (col, row)) = 
    if even col then
        Just (1 + row , 1 + col `div` 2)
    else
        Nothing

-- convert a coordinate to the Point type used by logic
coordToPoint :: Maybe (Int, Int) -> Maybe Lib.Point
coordToPoint p = case p of
    Just (i, j) -> Just Lib.Point {_i = i, _j = j}
    Nothing -> Nothing

-- addNotification :: GameState -> String -> GameState
-- TODO: send IP to network and return the result
connectWithOppo :: GameState -> Bool
connectWithOppo g = True

drawMyIP :: GameState -> Widget ResourceName
drawMyIP g = do
    -- TODO: get my IP from network
    str "My IP: " <+> str (g^.myIP)

-- draw opponent ip editor
drawEditor :: GameState -> Widget ResourceName
drawEditor g = str "Opponent's IP: " <+> (vLimit 1 edit)
    where edit = withFocusRing (g^.editFocus) (renderEditor (str . unlines)) (g^.opponentIP)

drawButton :: ResourceName -> String -> Widget ResourceName
drawButton r s = hCenterWith Nothing (clickable r $ border $ hCenterWith Nothing $ str s)

cursor :: GameState -> [T.CursorLocation ResourceName] -> Maybe (T.CursorLocation ResourceName)
cursor = focusRingCursor (^.editFocus)

getStonesList :: GameState -> [(Point, PointPattern)]
getStonesList g = 
    foldl (\l node -> l ++ [(fst node, stoneToPointPattern (snd node))]
        ) [] $ M.toList (g ^. boardState ^. Lib.board)
    -- M.foldlWithKey (\l k b -> 
    -- if b == Lib.Black || b == Lib.White then
    --     l ++ [(k, stoneToPointPattern b)]
    -- else
    --     l
    -- ) [] (g ^. boardState ^. Lib.board)

canModify :: Int -> Int -> (Int -> Int) -> Bool
canModify cur lim f = let res = f cur
    in if res <= lim && res >= 0 then
        True
    else
        False

-- Game Control: events, currently only handle resize event
handleEvent :: GameState -> BrickEvent ResourceName Tick -> EventM ResourceName (Next GameState)
handleEvent g (T.MouseDown Board BLeft _ loc) = do  -- left click to place stone
    let coord = inferCoordinate loc
    -- liftIO $ putStrLn (show coord)
        point = coordToPoint coord
    case point of
        Nothing -> continue $ g & (lastReportedClick .~ coord)
        Just p -> let {
                game = g ^. boardState;
                stone = game ^. Lib.player;
                msg = Lib.isValidMove game p stone;
                -- new_g = (boardState .~ new_board) g  -- set the boardState of GameState
                result = (L.isPrefixOf (show True) msg) && (L.isPrefixOf msg (show True))
                }
            in case result of
                True -> continue $ g
                    & (lastReportedClick .~ coord) 
                    & (boardState .~ (Lib.runMove (g ^. boardState) p stone)) 
                    & (totalRound %~ (+1)) & (currentRound %~ (+1))
                    & (\new_g -> new_g & snapshots %~ (++ [M.fromList $ getStonesList new_g]))
                _ ->  continue $ g & (lastReportedClick .~ coord) & (notification .~ msg)
handleEvent g (T.VtyEvent ev) = case ev of
    (EvKey KEsc []) -> halt g
    (EvKey KLeft []) -> continue =<<
        let f = (-) 1
        in case canModify (g^.currentRound) (g^.totalRound) f of
            True -> return (g & currentRound %~ f)
            _ -> return g
    (EvKey KRight []) -> continue =<<
        let f = (+) 1
        in case canModify (g^.currentRound) (g^.totalRound) f of
            True -> return (g & currentRound %~ f)
            _ -> return g
    _ -> continue =<< case focusGetCurrent (g^.editFocus) of
        Just IPField -> T.handleEventLensed g opponentIP handleEditorEvent ev
        _      -> return g
handleEvent g (T.MouseDown r _ _ _) = case r of
    ConnectButton -> do
        let submitStatus = connectWithOppo g
        let msg = if submitStatus == True then "Connection Success!" else "Connection Error. Please make sure you entered the correct IP address."
        continue $ g & (submitIP .~ submitStatus) & (notification .~ msg)
    ListenButton -> continue $ g & (notification .~ "Listening...")
    PassButton -> continue $ g & (notification .~ "Passed") -- TODO: add pass logic
    ResignButton -> continue $ g & (notification .~ "Opponent won") -- TODO: add resign logic
    _ -> continue g
handleEvent g (T.AppEvent e) = do
    continue $ g & (notification .~ "AppEvent generated")
handleEvent g _ = continue g

data EventType = CONNECT | SENDDATA | RECVDATA | DISCONNECT deriving (Eq, Ord)

data NetworkRequest = NetworkRequest {
    _eventType :: EventType,
    _requestSocket :: Maybe S.Socket,
    _action :: Either Lib.Point String
}

makeLenses ''NetworkRequest

data NetworkResponse = NetworkResponse {
    _result :: Bool,
    _responseSocket :: Maybe S.Socket,
    _msg :: String
}

makeLenses ''NetworkResponse

