{-# LANGUAGE TemplateHaskell #-}

module UI where

import Prelude as P
import Brick
    (Widget, BrickEvent, EventM
    , Edges(..)
    ,str
    ,halt, Next
    ,(<+>),(<=>), continue
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
    ,padLeftRight
    ,padAll
    ,padTop
    ,withBorderStyle
    )
import Brick.Widgets.Border
    (vBorder, hBorder, border, joinableBorder, borderWithLabel)
import Brick.Widgets.Border.Style
    (unicode, unicodeRounded, unicodeBold)
import qualified Brick.Types as T
import Brick.BChan
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
    when, void
    )
import Control.Monad.IO.Class (
    liftIO
    )
-- import Text.Read (
--     readMaybe
--     )
import GoLibrary as Lib
import NetworkInterface

import Control.Concurrent (forkIO)
import qualified Network.Socket as S
import Networks (requestHandler, getResponseMessage, getResponseStatus)
import qualified Data.List as L
import qualified Brick.Widgets.List as BL

data Tick = Tick
type NetworkEvent = (Either (Either NetworkRequest NetworkResponse) Tick)

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
    _listenSuccess :: Bool,
    _notification :: String,
    _socket :: Maybe S.Socket,
    _currentRound :: Int,  -- the round used for display,
    _totalRound :: Int,  -- total number of rounds
    _snapshots :: [M.Map Lib.Point PointPattern],
    _channel :: BChan NetworkEvent,
    _timer :: (Int, Int), -- countdown timer
    _timeIsUp :: Bool
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
getInitialState :: BChan NetworkEvent -> GameState
getInitialState chan = 
    let d = defaultBoardSize
        stone = Black
        points = [((Point x y), EmptyStone) | x <- [1..d], y <- [1..d]]
        s = GameState {
        _dim=d,  -- size of the board, need to be updated in appStartEvent
        _boardState = createGo stone d,
        _time=0,   -- how many rounds have passed
        _pointLocMap=buildPointLocMap d,  -- locations of black
        _lastReportedClick=Nothing,
        _opponentIP=(editor IPField Nothing ""),
        _editFocus=(focusRing [IPField]),
        _submitIP=False,
        _listenSuccess=False,
        _notification="Welcome to the Go game :)",
        _socket=Nothing,
        _currentRound=0,
        _totalRound=0,
        _snapshots=[M.fromList points],
        _channel=chan,
        _timer=(10, 0),
        _timeIsUp=False
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
drawIPInfo g = do
    if (g^.submitIP || g^.listenSuccess)
        then emptyWidget
        else vBox [strWrap "Please enter the opponent's IP and click CONNECT or LISTEN to connection request to start a game."
                ,str "To play white: click Connect"
                ,str "To play black: click Listen"
                ,hCenterWith (Just '-') (str "-")
                ,drawEditor g
                ,str "\n"
                ,(drawButton ConnectButton "Connect" <+> padAll 1 (str "OR") <+> drawButton ListenButton "Listen")
                ,str "\n"
                ,hBorder]

-- display some notification on the bottom panel
drawNotification :: GameState -> Widget ResourceName
drawNotification g = vBox [hCenterWith Nothing $ str "NOTIFICATION", str "\n", strWrap (g^.notification)]

-- display player/watchers information on the left panel
drawLeftColumn :: GameState -> Widget ResourceName
drawLeftColumn g = padAll 1 $ hLimit 30 $ vBox [
                    drawIPInfo g
                    ,drawNotification g]

-- draw the current board and other game info using GameState in the middle panel
-- currently an empty board is drawn using realDrawBoard
drawBoard :: GameState -> Widget ResourceName
drawBoard g = vBox [vLimit 1 $ hBox [hBorder, vBorder, padLeftRight 1 $ printRounds g, vBorder, hBorder]
                    ,hCenterWith Nothing $ str "Tip: Use Left/Right Key to view history"
                    ,realDrawBoard g
                    ,hCenterWith Nothing $ drawLastClick g
                    -- ,printMoveResults g
                    ]

printRounds :: GameState -> Widget ResourceName
printRounds g = str $ "Round: " ++ show (g ^. currentRound) ++ "/" ++ show ((P.length (g ^. snapshots)) - 1)

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

getCurrentMove :: GameState -> Lib.Stone
getCurrentMove g = let
    move = g^.boardState^.Lib.lastMove
    in case move of
        Pass s -> Lib.getOppositeStone s
        Move _ s -> Lib.getOppositeStone s

-- display some game stats on the right panel
-- TODO: change black move to dynamic + create timer
drawGameInfo :: GameState -> Widget ResourceName
drawGameInfo g = hLimit 30 $ vBox [padAll 1 $ hCenterWith Nothing $ str "Black move - " <+> drawCounter g
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
    Just (i, j) -> Just Lib.Point {Lib._i = i, Lib._j = j}
    Nothing -> Nothing

pointToCoord :: Maybe Lib.Point -> Maybe (Int, Int)
pointToCoord p = case p of
    Just Lib.Point {Lib._i = i, Lib._j = j} -> Just (i, j)
    Nothing -> Nothing

-- addNotification :: GameState -> String -> GameState
-- TODO: send IP to network and return the result
connectWithOppo :: GameState -> IO NetworkResponse
connectWithOppo g = do
    let oppoIP = head (getEditContents (g^.opponentIP))
    requestHandler (NetworkRequest CONNECT Nothing (Right oppoIP))

-- draw opponent ip editor
drawEditor :: GameState -> Widget ResourceName
drawEditor g = str "Opponent's IP: " <+> (vLimit 1 edit)
    where edit = withFocusRing (g^.editFocus) (renderEditor (str . unlines)) (g^.opponentIP)

drawButton :: ResourceName -> String -> Widget ResourceName
drawButton r s = hCenterWith Nothing (clickable r $ border $ hCenterWith Nothing $ str s)

drawCounter :: GameState -> Widget ResourceName
drawCounter g = do
    let (minute, sec) = g^.timer
    if sec < 10 && minute < 10
        then str $ "0" <> (show minute) <> ":" <> "0" <> (show sec)
    else if minute < 10
        then str $ "0" <> (show minute) <> ":" <> (show sec)
    else if sec < 10
        then str $ (show minute) <> ":" <> "0" <> (show sec)
    else str $ (show minute) <> ":" <> (show sec)
    

cursor :: GameState -> [T.CursorLocation ResourceName] -> Maybe (T.CursorLocation ResourceName)
cursor = focusRingCursor (^.editFocus)

-- UI: Whether handler can modify currentRound
canModify :: Int -> Int -> (Int -> Int) -> Bool
canModify cur lim f = let res = f cur
    in if res <= lim && res >= 0 then
        True
    else
        False

-- UI: generate board snapshot for storage
getStonesList :: GameState -> [(Point, PointPattern)]
getStonesList g = 
    foldl (\l node -> l ++ [(fst node, stoneToPointPattern (snd node))]
        ) [] $ M.toList (g ^. boardState ^. Lib.board)

processMove :: GameState -> Lib.Point -> Lib.Stone -> GameState
processMove g p stone = 
    let coord = Just (p^.Lib.i, p^.Lib.j)
    in g 
    & (lastReportedClick .~ coord) 
    & (boardState .~ (Lib.runMove (g ^. boardState) p stone))
    & (totalRound %~ (+1)) 
    & (currentRound %~ if (g^.totalRound) == (g^.currentRound) then
            (+) 1
        else
            (+) 0
        )
    & (\new_g -> new_g & snapshots %~ (++ [M.fromList $ getStonesList new_g]))

-- Game Control: events, currently only handle resize event
-- handleEvent :: GameState -> BrickEvent ResourceName Tick -> EventM ResourceName (Next GameState)
handleEvent :: GameState -> BrickEvent ResourceName NetworkEvent -> EventM ResourceName (Next GameState)
handleEvent g (T.MouseDown Board BLeft _ loc) = do  -- left click to place stone
    let coord = inferCoordinate loc
    -- liftIO $ putStrLn (show coord)
        point = coordToPoint coord
    case point of
        Nothing -> continue $ g & (lastReportedClick .~ coord)
        Just p -> let {
                game = g ^. boardState;
                stone = game ^. Lib.player;
                msg' = Lib.isValidMove game p stone;
                result' = (L.isPrefixOf (show True) msg') && (L.isPrefixOf msg' (show True))
                }
            in case result' of
                True -> do
                    _ <- (liftIO $ do
                        writeBChan (g ^. channel) (Left $ Left $ NetworkRequest {_eventType=SENDDATA, _requestSocket=g^.socket,_action=Left p}))
                    continue $ (processMove g p stone) & (timeIsUp .~ True) -- tells timer to reset
                False -> continue $ g & (notification .~ "Invalid move!")
handleEvent g (T.VtyEvent ev) = case ev of
    (EvKey KEsc []) -> halt g
    (EvKey KLeft []) -> continue =<< let f = (-) 1
        in case canModify (g^.currentRound) (g^.totalRound) f of
            True -> return $ g 
                & currentRound %~ f 
                & (notification .~ "Got Left arrow key pressed")
            _ -> return $ g 
    (EvKey KRight []) -> continue =<< let f = (+) 1
        in case canModify (g^.currentRound) (g^.totalRound) f of
            True -> return $ g 
                & currentRound %~ f 
                & notification .~ "Got Right arrow key pressed"
            _ -> return $ g 
    _ -> continue =<< case focusGetCurrent (g^.editFocus) of
        Just IPField -> T.handleEventLensed g opponentIP handleEditorEvent ev
        _      -> return g
handleEvent g (T.MouseDown r _ _ _) = case r of
    ConnectButton ->
        do
            _ <- liftIO $ do
                let oppoIP = head (getEditContents (g^.opponentIP))
                writeBChan (g^.channel) (Left $ Left $ NetworkRequest {_eventType=CONNECT, _requestSocket=g^.socket,_action=Right oppoIP})
            continue $ g & (submitIP .~ True)
    ListenButton -> do
        -- issue a network request
        _ <- liftIO $ do
            writeBChan (g^.channel) (Left $ Left $ NetworkRequest {_eventType=LISTEN,_requestSocket=g^.socket,_action=(Right "")})
        continue $ g & (notification .~ "Listening...")
    PassButton -> continue $ g & (notification .~ "Passed") -- TODO: add pass logic
    ResignButton -> continue $ g & (notification .~ "Opponent won") -- TODO: add resign logic
    _ -> continue g
-- deal with network requests in the channel
handleEvent g (T.AppEvent tickOrNetwork) = case tickOrNetwork of
    Left reqOrResp -> case reqOrResp of
        Left req -> do
            new_g <- liftIO $ handleNetworkRequest g req
            continue $ new_g
        Right resp -> 
            do
                new_g <- liftIO $ handleNetworkResponse g resp
                case (new_g^.socket) of
                    Nothing -> continue $ new_g & (listenSuccess .~ True)
                    Just _ -> do
                        -- write new recv request to the socket
                        _ <- liftIO $ do
                            writeBChan (new_g^.channel) (Left $ Left $ NetworkRequest {_eventType=RECVDATA, _requestSocket=new_g^.socket,_action=Right ""})
                        continue $ new_g & (listenSuccess .~ True)
    Right tick -> do
        let (minute, sec) = g^.timer
    
        if (g^.timeIsUp || not (g^.submitIP))
            then continue $ g & (timer .~ (10,0))
        else if minute == 0 && sec == 0
            then continue $ g & (timeIsUp .~ True) & (notification .~ "Time is up! Switching to opponent...") -- TODO: switch to opponent and set timeisup to false
        else if minute > 0 && sec == 0
            then if minute == 10
                then continue $ g & (timer .~ (minute-1,59)) & (notification .~ "Your turn to play! Timer has started.")
                else continue $ g & (timer .~ (minute-1,59))
        else continue $ g & (timer .~ (minute, sec-1))
handleEvent g _ = continue g

-- ===== Start of AppEvent handlers ====
-- client will be set to White, and all previous snapshots will be cleared
maybeChangeColor :: GameState -> NetworkRequest -> GameState
maybeChangeColor g req =
    case (req^.eventType) of
        CONNECT -> g 
            & (boardState %~ (& Lib.player .~ Lib.White))
            & (snapshots .~ [(g ^. snapshots) !! 0])
        _ -> g


-- NetworkRequest handler, the entrance function for various requests
handleNetworkRequest :: GameState -> NetworkRequest -> IO GameState
handleNetworkRequest g req = do
    _ <- liftIO $ forkIO $ do {
        resp <- requestHandler req;
        writeBChan (g^.channel) (Left $ Right resp);
        }
    return $ maybeChangeColor g req

handleNetworkResponse :: GameState -> NetworkResponse -> IO GameState
handleNetworkResponse g resp = do
    (\new_g -> case (resp^.msg) of
        Left point ->
            -- got a move from remote
            return $ processMove new_g point (Lib.getOppositeStone $ new_g^.boardState^.Lib.player)
        Right m ->
            return $ new_g & (notification .~ m)
        ) 
    $ g & (socket .~ (resp^.responseSocket)) & (notification .~ "Got new Move from remote")

-- ===== End of AppEvent handlers ====