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
    )
import Brick.Widgets.Center
    (hCenterWith, centerLayer
    )
import Brick.Widgets.Core 
    (hBox, vBox
    ,hLimit, vLimit
    ,withBorderStyle, joinBorders
    )
import Brick.Widgets.Border
    (vBorder, hBorder, border, joinableBorder)

import Brick.Widgets.Border.Style
    (unicode)

import GoLibrary as Lib

import Data.Map as M

data Tick = Tick

data PointPattern = BlackStone | WhiteStone | EmptyStone | HandicapPoint
data PointLoc = TopLeft | TopMid | TopRight | RightMid | BottomRight | BottomMid | BottomLeft | LeftMid | Other

-- The data structure owned by the UI part
data GameState = GameState { _Dim::Int, _BoardState::Lib.Game, _Time::Int, _PointLocMap:: M.Map Lib.Point PointLoc}

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
    let dim = defaultBoardSize
        stone = Black
    in GameState {
    _Dim=dim,  -- size of the board, need to be updated in appStartEvent
    _BoardState = Game{
        board = M.empty,
        boardSize = dim,
        player = stone,
        lastMove = Pass stone,
        scoreBlack = 0,
        scoreWhite = 0
    },
    _Time=0,   -- how many rounds have passed
    _PointLocMap=buildPointLocMap dim  -- locations of black
}

decidePointLoc :: GameState -> Int -> Int -> PointLoc
decidePointLoc (GameState d s t m) i j = 
    let pl = M.lookup (Point i j) m
    in case pl of
        Nothing -> Other
        Just loc -> loc

decideBoardDim :: GameState -> Int
decideBoardDim (GameState d s t m) = d

decidePointPattern :: GameState -> Int -> Int -> PointPattern
decidePointPattern g i j = EmptyStone

type ResourceName = ()

-- Game Visualizer
-- the entrance function of drawing
drawUi :: GameState -> [Widget ()]
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
drawRoomInfo :: GameState -> Widget ()
drawRoomInfo g = vLimit 1 $ hBox [hBorder, vBorder, str " Test room ", vBorder, hBorder]

-- display player/watchers information on the left panel
drawPlayerInfo :: GameState -> Widget ()
drawPlayerInfo g = hLimit 30 $ str "Black: " <=> str "White: " <=> str "Other info: number of watchers"

-- draw the current board and other game info using GameState in the middle panel
-- currently an empty board is drawn using realDrawBoard
drawBoard :: GameState -> Widget ()
drawBoard g = vBox [vLimit 1 $ hCenterWith (Just '-') $ hBox [vBorder, str " Board ", vBorder]
                    ,realDrawBoard g
                    ,hCenterWith Nothing (str "Round: 0")
                    ]

-- helper function, either draw a stone/empty intersection point
-- currently only draws empty intersection point
stoneOrEmpty :: PointPattern -> PointLoc -> Widget ()
stoneOrEmpty s TopLeft = joinableBorder $ Edges {eTop=False, eBottom=True, eLeft=False, eRight=True}
stoneOrEmpty s TopMid = joinableBorder $ Edges {eTop=False, eBottom=True, eLeft=True, eRight=True}
stoneOrEmpty s TopRight = joinableBorder $ Edges {eTop=False, eBottom=True, eLeft=True, eRight=False}
stoneOrEmpty s LeftMid = joinableBorder $ Edges {eTop=True, eBottom=True, eLeft=False, eRight=True}
stoneOrEmpty s RightMid = joinableBorder $ Edges {eTop=True, eBottom=True, eLeft=True, eRight=False}
stoneOrEmpty s BottomLeft = joinableBorder $ Edges {eTop=True, eBottom=False, eLeft=False, eRight=True}
stoneOrEmpty s BottomMid = joinableBorder $ Edges {eTop=True, eBottom=False, eLeft=True, eRight=True}
stoneOrEmpty s BottomRight = joinableBorder $ Edges {eTop=True, eBottom=False, eLeft=True, eRight=False}
stoneOrEmpty s Other = joinableBorder $ Edges {eTop=True, eBottom=True, eLeft=True, eRight=True}

-- helper function, either draw a stone/empty intersection point using `stoneOrEmpty`
-- deal with rightmost column as they don't need an extra hborder on their right.
basicGrid :: PointPattern -> PointLoc -> Widget ()
basicGrid s l = case l of
    TopRight -> stoneOrEmpty s l
    RightMid -> stoneOrEmpty s l
    BottomRight -> stoneOrEmpty s l
    _ -> hBox [stoneOrEmpty s l, h_border]
        where
            h_border = joinableBorder $ Edges {eTop=False, eBottom=False, eLeft=True, eRight=True}

-- Literally, do the real job to draw the board
realDrawBoard :: GameState -> Widget ()
realDrawBoard g = centerLayer $
    vBox
        [(hBox [(basicGrid (decidePointPattern g i j) (decidePointLoc g i j)) | 
                j <- [1..(decideBoardDim g)]]) |
            i <- [1..(decideBoardDim g)]
        ]

-- display some game stats on the right panel
drawGameInfo :: GameState -> Widget ()
drawGameInfo g = hLimit 30 $ str "Timer / Points / Game Result"

-- display some notification on the bottom panel
drawNotification :: GameState -> Widget ()
drawNotification g = str "Notifications will appear here"


-- Game Control: events, currently only handle resize event
handleEvent :: GameState -> BrickEvent () Tick -> EventM () (Next GameState)
handleEvent g e = resizeOrQuit g e
