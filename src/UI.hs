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

import GoLibrary
    (Game
    ,Stone
    ,GoBoard
    )
import Data.Map as M
-- import Graphics.Vty as V

data Tick = Tick

data Point = Point Int Int deriving (Ord, Eq)
data PointState = Black | White | Empty | Handicap
data PointLoc = TopLeft | TopMid | TopRight | LeftMid | RightMid | BottomLeft | BottomMid | BottomRight | Other

-- The data structure owned by UI, the UI part
data GameState = GameState { _Dim::Int, _BoardState::Game, _Time::Int, _PointLocMap:: M.Map Point PointLoc}

-- pointLocMap is used to decide how to render empty intersections on different parts of the board
buildPointLocMap :: Int -> M.Map Point PointLoc
buildPointLocMap l = M.fromList [((Point x y), Other) |
    i <- [2..(l-1)],
    j <- [2..(l-1)]
    ]

decideLoc :: Int -> Int -> l -> PointLoc
decideLoc x y l = 
    

type ResourceName = ()

sideLength :: Integer
sideLength = 10

-- Game Visualizer
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

drawRoomInfo :: GameState -> Widget ()
drawRoomInfo g = vLimit 1 $ hBox [hBorder, vBorder, str " Test room ", vBorder, hBorder]

drawPlayerInfo :: GameState -> Widget ()
drawPlayerInfo g = hLimit 30 $ str "Black: " <=> str "White: " <=> str "Other info: number of watchers"

drawBoard :: GameState -> Widget ()
drawBoard g = vBox [vLimit 1 $ hCenterWith (Just '-') $ hBox [vBorder, str " Board ", vBorder]
                    ,realDrawBoard g
                    ,hCenterWith Nothing (str "Round: 0")
                    ]


stoneOrEmpty :: PointState -> PointLoc -> Widget ()
stoneOrEmpty s TopLeft = joinableBorder $ Edges {eTop=False, eBottom=True, eLeft=False, eRight=True}
stoneOrEmpty s TopMid = joinableBorder $ Edges {eTop=False, eBottom=True, eLeft=True, eRight=True}
stoneOrEmpty s TopRight = joinableBorder $ Edges {eTop=False, eBottom=True, eLeft=True, eRight=False}
stoneOrEmpty s LeftMid = joinableBorder $ Edges {eTop=True, eBottom=True, eLeft=False, eRight=True}
stoneOrEmpty s RightMid = joinableBorder $ Edges {eTop=True, eBottom=True, eLeft=True, eRight=False}
stoneOrEmpty s BottomLeft = joinableBorder $ Edges {eTop=True, eBottom=False, eLeft=False, eRight=True}
stoneOrEmpty s BottomMid = joinableBorder $ Edges {eTop=True, eBottom=False, eLeft=True, eRight=True}
stoneOrEmpty s BottomRight = joinableBorder $ Edges {eTop=True, eBottom=False, eLeft=True, eRight=False}
stoneOrEmpty s Other = joinableBorder $ Edges {eTop=True, eBottom=True, eLeft=True, eRight=True}

basicGrid :: PointState -> PointLoc -> Widget ()
basicGrid s l = withBorderStyle unicode $
    vBox [
        first_row
        ,last_row
    ]
    where
        first_row = hBox [stoneOrEmpty s l, h_border]
        last_row = hBox [v_border, hBorder]
        h_border = joinableBorder $ Edges {eTop=False, eBottom=False, eLeft=True, eRight=True}
        v_border = joinableBorder $ Edges {eTop=True, eBottom=True, eLeft=False, eRight=False}

decideBoardDim :: GameState -> Int
decideBoardDim g = 19

decidePointLoc :: GameState -> Int -> Int -> PointLoc
decidePointLoc g i j = 
    lookup (i, j) PointLocMap
    -- let l = decideBoardDim g
    -- in 

decidePointState :: GameState -> Int -> Int -> PointState
decidePointState g i j = Empty

realDrawBoard :: GameState -> Widget ()
realDrawBoard g = centerLayer $
    [hBox [vBox [basicGrid (decidePointState g i j) (decidePointLoc g i j) |
            i <- [1..(decideBoardDim g)],
            j <- [1..(decideBoardDim g)]
            ]
        ]
    ]
        
    -- joinBorders $ hBox $ [first_row, mid_rows, last_row]
    -- where
    --     l = 19
    --     first_row = vBox [tl_corner, mid_grids, tr_corner]
    --     tl_corner = basicGrid Empty TopLeft
    --     mid_grids = hBox $ take (l-2) $ repeat $ basicGrid Empty TopMid
    --     tr_corner = basicGrid Empty TopRight

    --     mid_rows = hBox $ take (l-2) $ repeat mid_row
    --     mid
    --     row = hBox $ (take (l - 1) $ repeat $ basicGrid l) ++ [last_grid_in_row]
    --     last_grid_in_row = hBox [stoneOrEmpty Empty LastCol]

        -- last_row = vBox $ take (l - 1) $ repeat row

drawGameInfo :: GameState -> Widget ()
drawGameInfo g = hLimit 30 $ str "Timer / Points / Game Result"

drawNotification :: GameState -> Widget ()
drawNotification g = str "Notifications will appear here"

-- Game Control: events

handleEvent :: GameState -> BrickEvent () Tick -> EventM () (Next GameState)
handleEvent g e = resizeOrQuit g e