{-# LANGUAGE TemplateHaskell #-}

module UI where

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
import Brick.Widgets.Center
    (hCenterWith, centerLayer
    )
import Brick.Widgets.Core 
    (hBox, vBox
    ,hLimit, vLimit
    ,withBorderStyle, joinBorders
    ,clickable
    )
import Brick.Widgets.Border
    (vBorder, hBorder, border, joinableBorder)
import Brick.Widgets.Border.Style
    (unicode)
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
import Data.Map as M
import Control.Monad (
    when
    )
import Control.Monad.IO.Class (
    liftIO
    )
import GoLibrary as Lib


data Tick = Tick

data ResourceName = Board | OtherResources deriving (Show, Ord, Eq)


data PointPattern = BlackStone | WhiteStone | EmptyStone | HandicapPoint
data PointLoc = TopLeft | TopMid | TopRight | RightMid | BottomRight | BottomMid | BottomLeft | LeftMid | Other

stoneToPointPattern :: Stone -> PointPattern
stoneToPointPattern Black   = BlackStone
stoneToPointPattern White   = WhiteStone
stoneToPointPattern Ko      = EmptyStone
stoneToPointPattern Empty   = EmptyStone

-- The data structure owned by the UI part
-- Prefix the fields in GameState with underscore "_" to make lenses for it
data GameState = GameState { 
    _dim::Int, 
    _boardState::Lib.Game, 
    _time::Int, 
    _pointLocMap:: M.Map Lib.Point PointLoc,
    _lastReportedClick :: Maybe (Int, Int)}

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
--      over (dim . s) (\_ -> 19)


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
        s = GameState {
        _dim=d,  -- size of the board, need to be updated in appStartEvent
        _boardState = Game{
            board = M.empty,
            boardSize = d,
            player = stone,
            lastMove = Pass stone,
            scoreBlack = 0,
            scoreWhite = 0
        },
        _time=0,   -- how many rounds have passed
        _pointLocMap=buildPointLocMap d,  -- locations of black
        _lastReportedClick=Nothing
        }
    in s

decidePointLoc :: GameState -> Int -> Int -> PointLoc
decidePointLoc g i j = 
    let pl = M.lookup (Point i j) (g ^. pointLocMap)
    in case pl of
        Nothing -> Other
        Just loc -> loc

isBlackStone :: GameState -> Int -> Int -> Bool
isBlackStone g i j = False -- view (boardState . g)

isWhiteStone :: GameState -> Int -> Int -> Bool
isWhiteStone g i j = False

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
    if isBlackStone g i j then
        BlackStone
    else if isWhiteStone g i j then
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
        drawRoomInfo g
        ,hBox [drawPlayerInfo g
            ,vBorder
            ,drawBoard g
            ,vBorder
            ,drawGameInfo g
            ]
        ,drawNotification g
        ]
    ]

-- display game room name on the top
drawRoomInfo :: GameState -> Widget ResourceName
drawRoomInfo g = vLimit 1 $ hBox [hBorder, vBorder, str " Test room ", vBorder, hBorder]

-- display player/watchers information on the left panel
drawPlayerInfo :: GameState -> Widget ResourceName
drawPlayerInfo g = hLimit 30 $ str "Black: " <=> str "White: " <=> str "Other info: number of watchers"

-- draw the current board and other game info using GameState in the middle panel
-- currently an empty board is drawn using realDrawBoard
drawBoard :: GameState -> Widget ResourceName
drawBoard g = vBox [vLimit 1 $ hCenterWith (Just '-') $ hBox [vBorder, str " Board ", vBorder]
                    ,realDrawBoard g
                    ,drawLastClick g
                    ,hCenterWith Nothing (str "Round: 0")
                    ]

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

-- display some game stats on the right panel
drawGameInfo :: GameState -> Widget ResourceName
drawGameInfo g = hLimit 30 $ str "Timer / Points / Game Result"

-- display some notification on the bottom panel
drawNotification :: GameState -> Widget ResourceName
drawNotification g = str "Notifications will appear here"

inferCoordinate :: T.Location -> Maybe (Int, Int)
inferCoordinate (T.Location (col, row)) = 
    if even col then
        Just (1 + row , 1 + col `div` 2)
    else
        Nothing

-- Game Control: events, currently only handle resize event
handleEvent :: GameState -> BrickEvent ResourceName Tick -> EventM ResourceName (Next GameState)
handleEvent g (T.MouseDown Board BLeft _ loc) = do  -- left click to place stone
    let coord = inferCoordinate loc
    -- liftIO $ putStrLn (show coord)
    continue $ g & (lastReportedClick .~ coord)
handleEvent g (T.VtyEvent (EvKey KEsc [])) = halt g
handleEvent g e = continue g