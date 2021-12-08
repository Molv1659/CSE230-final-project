{-# LANGUAGE TemplateHaskell #-}

{-
    in UI connect: use createGo to initialize the Go game

    in UI click goboard: first use isValidMove to check, if yes use runMove, else show the error string

    in UI Pass button: first use isValidMove to check, if yes then runPass, then use checkTwoPass to 
    see whether the game should end, if yes, use finishGo to calculate the scores and getWinner
-}
module GoLibrary(
    Point(Point),
    Stone(Black, White, Ko, Empty),
    GoBoard,
    Move(Pass, Move),
    Game(Game),
    createGo,
    isValidMove,
    runMove,
    isValidPass,
    runPass,
    checkTwoPass,   
    finishGo,
    getWinner
) where


import qualified Data.Map as Map
import qualified Data.List as List

import Lens.Micro.TH (
    makeLenses
    )
import Lens.Micro (
    over
    , (^.), (&), (.~), (%~)
    )

data Point = Point {_i :: Int, _j :: Int}
                deriving (Ord, Eq)

data Stone = Black | White | Ko | Empty 
                deriving (Eq, Show)

type GoBoard = Map.Map Point Stone

data Move = Pass Stone | Move Point Stone 
                deriving (Eq)

data Game = Game { 
    _board :: GoBoard,
    _boardSize :: Int,
    _player :: Stone,
    _lastMove :: Move,
    _moveHistory :: [Move],
    _scoreBlack :: Int,
    _scoreWhite :: Int
}

makeLenses ''Point
makeLenses ''Game
-- give size and stone, create the go game. one player initial with black, another initial with white
createGo :: Stone -> Int -> Game
createGo this_player size = Game{
    _board = putStones' Map.empty points Empty,
    _boardSize = size,
    _player = this_player,
    _lastMove = Pass White,
    _moveHistory = [],
    _scoreBlack = 0,
    _scoreWhite = 0
} where points = [(Point x y) | x <- [1..size], y <- [1..size]]

-- get the go board
getBoard :: Game -> GoBoard
getBoard game@(Game b bz pl lm mh sb sw) = b

-- get the go board size
getSize :: Game -> Int
getSize game@(Game b bz pl lm mh sb sw) = bz

-- get the stone at Point x y
getStone :: Game -> Point -> Stone
getStone game@(Game b bz pl lm mh sb sw) point = Map.findWithDefault Empty  point b

-- get game move history
getMoveHistory :: Game -> [Move]
getMoveHistory game@(Game b bz pl lm mh sb sw) = mh

-- run a move at (x, y), should check validity first
runMove :: Game -> Point -> Stone -> Game
runMove game point stone = removePrisoners (putStone (unlabelAllKo game) point stone) point (getOppositeStone stone)

-- player use pass to show he/she think no more scores can be get
runPass :: Game -> Stone -> Game
runPass game@(Game b bz pl lm mh sb sw) stone = Game{
    _board = b,
    _boardSize = bz,
    _player = pl,
    _lastMove = Pass stone,
    _moveHistory = mh ++ [Pass stone],
    _scoreBlack = if stone == Black then sb else (sb + 1),
    _scoreWhite = if stone == White then sw else (sw + 1)
}

-- after runPass, use checkTwoPass to see whether the game end, if yes, use finishGo to calculate the score and getWinner 
checkTwoPass :: Game -> Bool
checkTwoPass game@(Game b bz pl lm mh sb sw) = ((getPassNum mh)== 2)

getPassNum :: [Move] -> Int
getPassNum []            = 0
getPassNum ((Pass _):ms)   = 1 + getPassNum ms
getPassNum ((Move _ _):ms) = getPassNum ms
        

-- check whether a move is valid
isValidMove :: Game -> Point -> Stone -> String
isValidMove game@(Game b bz pl lm mh sb sw) point stone 
    | not ((stone == turnPlayer game) && (stone == pl)) = "Not your turn."
    | Map.findWithDefault Empty point b /= Empty        = "Please put the stone on empty place."
    | length (findDead game point stone) == 0           = "True"
    | length removeAll /= 0                             = "True"
    | otherwise                                         = "You cannot put stone here. You're turning your stones into prisoners!"
    where
        removeAll   = removeUp ++ removeDown ++ removeLeft ++ removeRight
        removeUp    = findDead game (upPoint point) prison_stone 
        removeDown  = findDead game (downPoint point) prison_stone
        removeLeft  = findDead game (leftPoint point) prison_stone
        removeRight = findDead game (rightPoint point) prison_stone
        prison_stone = getOppositeStone stone

isValidPass :: Game -> Stone ->String
isValidPass game@(Game b bz pl lm mh sb sw) stone
    | not ((stone == turnPlayer game) && (stone == pl)) = "Not your turn."
    | otherwise                                         = "True"

-- after both player used pass, the game end, use finishGo to count scores
finishGo :: Game -> Game
finishGo game@(Game b bz pl lm mh sb sw) = Game {
    _board = b,
    _boardSize = bz,
    _player = pl,
    _lastMove = lm,
    _moveHistory = mh,
    _scoreBlack = sb + blackTerritory,
    _scoreWhite = sw + whiteTerritory
} where (blackTerritory, whiteTerritory) = countTerritory game

-- after finishGo, use getWinner
getWinner :: Game -> Stone
getWinner game@(Game b bz pl lm mh sb sw)
    | sb > sw + 7 = Black
    | otherwise   = White





-----------------------------------------------------------------------------
turnPlayer :: Game -> Stone
turnPlayer game@(Game b bz pl lm mh sb sw) = case lm of Pass stone -> getOppositeStone stone
                                                        Move point stone -> getOppositeStone stone


removePrisoners :: Game -> Point -> Stone -> Game
removePrisoners game point prison_stone
    | length removeAll == 1 && isKo = updateScore (labelKo (removeStones game removeAll) (removeAll !! 0)) 1 attack_stone
    | otherwise = updateScore (removeStones game removeAll) (length removeAll) attack_stone
    where 
        removeAll   = removeUp ++ removeDown ++ removeLeft ++ removeRight
        removeUp    = findDead game (upPoint point) prison_stone 
        removeDown  = findDead game (downPoint point) prison_stone
        removeLeft  = findDead game (leftPoint point) prison_stone
        removeRight = findDead game (rightPoint point) prison_stone
        ko_check_point = if length removeUp == 1 then (upPoint point) 
                        else if length removeDown == 1 then (downPoint point)
                        else if length removeLeft == 1 then (leftPoint point)
                        else if length removeRight == 1 then (rightPoint point)
                        else Point (-1) (-1)
        attack_stone = getOppositeStone prison_stone
        isKo = (ko_check_point /= Point (-1) (-1)) && (length removeAll' == 1)
        removeAll'   = removeUp' ++ removeDown' ++ removeLeft' ++ removeRight'
        removeUp'    = findDead game (upPoint ko_check_point) attack_stone
        removeDown'  = findDead game (downPoint ko_check_point) attack_stone
        removeLeft'  = findDead game (leftPoint ko_check_point) attack_stone
        removeRight' = findDead game (rightPoint ko_check_point) attack_stone

upPoint :: Point -> Point
upPoint (Point x y) = Point (x-1) y

downPoint :: Point -> Point
downPoint (Point x y) = Point (x+1) y

leftPoint :: Point -> Point
leftPoint (Point x y) = Point x (y-1)

rightPoint :: Point -> Point
rightPoint (Point x y) = Point x (y+1)

putStone :: Game -> Point -> Stone -> Game
putStone game@(Game b bz pl lm mh sb sw) point stone = Game{
    _board = putStone' b point stone,
    _boardSize = bz,
    _player = pl,
    _lastMove = Move point stone,
    _moveHistory = mh ++ [Move point stone],
    _scoreBlack = sb,
    _scoreWhite = sw
}

removeStone :: Game -> Point -> Game
-- removeStone game point = (_board .~ (putStone' b point Empty)) game
removeStone game@(Game b bz pl lm mh sb sw) point = Game{
    _board = putStone' b point Empty,
    _boardSize = bz,
    _player = pl,
    _lastMove = lm,
    _moveHistory = mh,
    _scoreBlack = sb,
    _scoreWhite = sw
}



removeStones :: Game -> [Point] -> Game
removeStones game []      = game
removeStones game (p:ps) = removeStones (removeStone game p) ps

labelKo :: Game -> Point -> Game
-- labelKo game point = (_board .~ (putStone' b point Ko)) game
labelKo game@(Game b bz pl lm mh sb sw) point = Game{
    _board = putStone' b point Ko,
    _boardSize = bz,
    _player = pl,
    _lastMove = lm,
    _moveHistory = mh,
    _scoreBlack = sb,
    _scoreWhite = sw
}

unlabelAllKo :: Game -> Game
-- unlabelAllKo game = (_board .~ (if ko_points == [] then b else putStones' b (fmap fst ko_points) Empty)) game
--     where ko_points = List.filter (\(p, s) -> s == Ko) (Map.assocs b)
unlabelAllKo game@(Game b bz pl lm mh sb sw)= Game{
    _board = if ko_points == [] then b else putStones' b (fmap fst ko_points) Empty,
    _boardSize = bz,
    _player = pl,
    _lastMove = lm,
    _moveHistory = mh,
    _scoreBlack = sb,
    _scoreWhite = sw
} where ko_points = List.filter (\(p, s) -> s == Ko) (Map.assocs b)


putStone' :: GoBoard -> Point -> Stone -> GoBoard
putStone' board point stone = Map.insert point stone (Map.delete point board)

putStones' :: GoBoard -> [Point] -> Stone -> GoBoard
putStones' board [] stone = board
putStones' board (p:ps) stone = putStones' (putStone' board p stone) ps stone

getOppositeStone :: Stone -> Stone
getOppositeStone stone
    | stone == Black = White
    | stone == White = Black
    | otherwise = Empty

updateScore :: Game -> Int -> Stone -> Game
updateScore game@(Game b bz pl lm mh sb sw) score stone
    | stone == Black = (Game b bz pl lm mh (sb + score) sw)
    | otherwise      = (Game b bz pl lm mh sb (sw + score))

findDead :: Game -> Point -> Stone -> [Point]
findDead game point prison_stone
    | elem Nothing bfs_result = []
    | otherwise = purifyPoints bfs_result
    where bfs_result =  bfsSearchDead game point prison_stone []

purify :: Maybe a -> a
purify (Just a) = a

purifyPoints :: [Maybe Point] -> [Point]
purifyPoints [] = []
purifyPoints ((Just p):ps) = p:(purifyPoints ps)

bfsSearchDead :: Game -> Point -> Stone -> [Maybe Point] -> [Maybe Point]
bfsSearchDead game@(Game b bz pl lm mh sb sw) point@(Point x y) prison_stone seen_points
    | x < 1 || x > bz || y < 1 || y > bz             = seen_points
    | elem (Just point) seen_points                  = seen_points
    | Map.findWithDefault Empty point b == Empty                   = Nothing:seen_points
    | Map.findWithDefault Empty point b == Ko                      = Nothing:seen_points
    | Map.findWithDefault Empty point b /= prison_stone            = seen_points
    | otherwise = bfsSearchDead game (upPoint point) prison_stone
        $ bfsSearchDead game (downPoint point) prison_stone
        $ bfsSearchDead game (leftPoint point) prison_stone
        $ bfsSearchDead game (rightPoint point) prison_stone
        ((Just point):seen_points)



countTerritory :: Game -> (Int, Int)
countTerritory game = (blackTerritory, whiteTerritory)
    where
        blackTerritory = length $ List.filter (\x -> x == Black) territory
        whiteTerritory = length $ List.filter (\x -> x == White) territory
        territory = fmap (decideTerritory game) points
        points = [(Point x y) | x <- [1..size], y <- [1..size]]
        size = getSize game

decideTerritory :: Game -> Point -> Stone
decideTerritory game@(Game b bz pl lm mh sb sw) point
    | Map.findWithDefault Empty  point b == Black  = Black
    | Map.findWithDefault Empty  point b == White  = White
    | blackNeighbor > whiteNeighber = Black
    | blackNeighbor < whiteNeighber = White
    | otherwise                     = Empty
    where 
        blackNeighbor = countNeighbor game point Black
        whiteNeighber = countNeighbor game point White

countNeighbor :: Game -> Point -> Stone -> Int
countNeighbor game point stone = countAll 
    where  
        countAll = countUp + countDown + countLeft + countRight
        countUp = countOneDirection game point (-1, 0) stone
        countDown = countOneDirection game point (1, 0) stone
        countLeft = countOneDirection game point (0, -1) stone
        countRight = countOneDirection game point (0, 1) stone

countOneDirection :: Game -> Point -> (Int, Int) -> Stone -> Int
countOneDirection game@(Game b bz pl lm mh sb sw) point@(Point x y) (xx, yy) stone
    | Map.findWithDefault Empty newpoint b == Empty                  = 0
    | Map.findWithDefault Empty newpoint b == stone                  = 1
    | Map.findWithDefault Empty newpoint b == getOppositeStone stone = 0
    | otherwise                                        = countOneDirection game newpoint (xx, yy) stone
    where
        newpoint = Point (x + xx) (y + yy)




