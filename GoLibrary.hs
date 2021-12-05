module GoLibrary(
    Point(Point),
    Stone(Black, White, Ko, Empty),
    GoBoard,
    Move(Pass, Move),
    Game(Game),
    createGo,
    getBoard,
    getStone,
    getMoveHistory,
    runMove,
    runPass,
    isValidMove,
    finishGo,
    getWinner
) where

import Data.Map as Map

data Point = Point Int Int deriving (Ord, Eq)

data Stone = Black | White | Ko | Empty deriving (Eq, Show)

data GoBoard = Map Point Stone

data Move = Pass Stone | Move Point Stone deriving (Eq)

data Game = Game { 
    board :: GoBoard,
    boardSize :: Int,
    player :: Stone,
    lastMove :: Move,
    moveHistory :: [Move],
    scoreBlack :: Int,
    scoreWhite :: Int
}

-- give size and stone, create the go game. one player initial with black, another initial with white
createGo :: Stone -> Int -> Game
createGo player size = Game{
    board = putStones' points Empty,
    boardSize = size,
    player = player,
    lastMove = Pass White,
    moveHistory = [],
    scoreBlack = 0,
    scoreWhite = 0
} where points = [(Point x y) | x <- [1..size], y <- [1..size]]

-- get the go board
getBoard :: Game -> GoBoard
getBoard game@(Game b bz pl lm mh sb sw) = b

-- get the stone at Point x y
getStone :: Game -> Point -> Stone
getStone game@(Game b bz pl lm mh sb sw) point = Map.findKey point b

-- get game move history
getMoveHistory :: Game -> [Move]
getMoveHistory game@(Game b bz pl lm mh sb sw) point = mh

-- run a move at (x, y), should check validity first
runMove :: Game -> Point -> Stone -> Game
runMove game point stone = removePrisoners (putStone (unlabelAllKo game) point stone) point (getOppositeStone stone)

-- player use pass to show he/she think no more scores can be get
runPass :: Game -> Game
runPass game@(Game b bz pl lm mh sb sw) stone = Game{
    board = b,
    boardSize = size,
    player = pl,
    lastMove = Pass stone,
    moveHistory = mh ++ [Pass stone],
    scoreBlack = if stone == Black then sb else (sb + 1),
    scoreWhite = if stone == While then sw else (sw + 1)
}

-- check whether a move is valid
isValidMove :: Game -> Point -> Stone -> Bool
isValidMove game@(Game b bz pl lm mh sb sw) point stone 
    | not (stone == turnPlayer && stone == pl) = False
    | Map.findKey point b != Empty             = False
    | length (findDead game point stone) == 0  = True
    | length removeAll /= 0                    = True
    | otherwise                                = False
    where
        removeAll   = removeUp ++ removeDown ++ removeLeft ++ removeRight
        removeUp    = findDead game (upPoint point) prison_stone 
        removeDown  = findDead game (downPoint point) prison_stone
        removeLeft  = findDead game (leftPoint point) prison_stone
        removeRight = findDead game (rightPoint point) prison_stone
        prison_stone = getOppositeStone stone

-- after both player used pass, the game end, use finishGo to count scores
finishGo :: Game -> Game
finishGo game@(Game b bz pl lm mh sb sw) = Game {
    board = b,
    boardSize = size,
    player = pl,
    lastMove = lm,
    moveHistory = mh,
    scoreBlack = sb + blackTerritory,
    scoreWhite = sw + whiteTerritory
} where (blackTerritory, whiteTerritory) = countTerritory game

-- after finishGo, use getWinner
getWinner :: Game -> Stone
getWinner game@(Game b bz pl lm mh sb sw)
    | sb > sw + 7 = Black
    | otherwise   = White


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
                        else Point -1 -1
        attack_stone = getOppositeStone prison_stone
        is_ko = (ko_check_point /= Point -1 -1) && (length removeAll' == 1)
        removeAll'   = removeUp' ++ removeDown' ++ removeLeft' ++ removeRight'
        removeUp'    = findDead game (upPoint ko_check_point) attack_stone
        removeDown'  = findDead game (downPoint ko_check_point) attack_stone
        removeLeft'  = findDead game (leftPoint ko_check_point) attack_stone
        removeRight' = findDead game (rightPoint ko_check_point) attack_stone

upPoint :: Point -> Point
upPoint (Point x y) = Point x-1 y

downPoint :: Point -> Point
downPoint (Point x y) = Point x+1 y

leftPoint :: Point -> Point
leftPoint (Point x y) = Point x y-1

rightPoint :: Point -> Point
rightPoint (Point x y) = Point x y+1

putStone :: Game -> Point -> Stone -> Game
putStone game@(Game b bz pl lm mh sb sw) point stone = Game{
    board = putStone' b point stone,
    boardSize = bz,
    player = pl,
    lastMove = Move point stone,
    moveHistory = mh ++ [Move point stone],
    scoreBlack = sb,
    scoreWhite = sw
}

removeStone :: Game -> Point -> Game
removeStone game@(Game b bz pl lm mh sb sw) point stone = Game{
    board = putStone' b point Empty,
    boardSize = bz,
    player = pl,
    lastMove = lm,
    moveHistory = mh,
    scoreBlack = sb,
    scoreWhite = sw
}

removeStones :: Game -> [Point] -> Game
removeStone game []      = game
removeStones game (p:ps) = removeStones (removeStone game p) ps

labelKo :: Game -> Point -> Game
labelKo game@(Game b bz pl lm mh sb sw) point stone = Game{
    board = putStone' b point Ko,
    boardSize = bz,
    player = pl,
    lastMove = lm,
    moveHistory = mh,
    scoreBlack = sb,
    scoreWhite = sw
}

unlabelAllKo :: Game -> Game
unlabelAllKo game@(Game b bz pl lm mh sb sw) point stone = Game{
    board = if ko_points == [] then b else putStone' b (fmap fst ko_points) Empty,
    boardSize = bz,
    player = pl,
    lastMove = lm,
    moveHistory = mh,
    scoreBlack = sb,
    scoreWhite = sw
} where ko_points = List.filter (\(p, s) -> s == Ko) (assocs b)


putStone' :: GoBoard -> Point -> Stone -> GoBoard
putStone' board point stone = Map.insert point stone (Map.delete point board)

putStones' :: GoBoard -> [Point] -> Stone -> GoBoard
putStones' board [] stone = board
putStones' board [p:ps] stone = putStones' (putStone' board p stone) ps stone

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
findDead game point prison_stone = 
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
    | Map.findKey point b == Empty                   = Nothing:seen_points
    | Map.findKey point b == Ko                      = Nothing:seen_points
    | Map.findKey point b /= prison_stone            = seen_points
    | otherwise = bfsSearchDead game (upPoint point)
        $ bfsSearchDead game (downPoint point)
        $ bfsSearchDead game (leftPoint point)
        $ bfsSearchDead game (rightPoint point)
        ((Just point):seen_points)

countTerritory :: Game -> (Int, Int)
countTerritory game = (blackTerritory, whiteTerritory)
    where
        blackTerritory = length $ filter (\x == Black) territory
        whiteTerritory = length $ filter (\x == White) territory
        territory = fmap (decideTerritory game) points
        points = [(Point x y) | x <- [1..size], y <- [1..size]]

decideTerritory :: Game -> Point -> Stone
decideTerritory game@(Game b bz pl lm mh sb sw) point
    | Map.findKey point b == Black  = Black
    | Map.findKey point b == White  = White
    | blackNeighbor > whiteNeighber = Black
    | blackNeighbor < whiteNeighber = White
    | otherwise                     = Empty
    where 
        blackNeighbor = countNeighbor game point Black
        whiteNeighber = countNeighbor game point White

countNeighbor :: Game -> Point -> Stone
countNeighbor game point stone = countAll 
    where  
        countAll = countUp + countDown + countLeft + countRight
        countUp = countOneDirection game point (-1, 0) stone
        countDown = countOneDirection game point (1, 0) stone
        countLeft = countOneDirection game point (0, -1) stone
        countRight = countOneDirection game point (0, 1) stone

countOneDirection :: Game -> Point -> (Int, Int) -> Stone
countOneDirection game@(Game b bz pl lm mh sb sw) point@(Point x y) (xx, yy) stone
    | Map.findKey newpoint b == Nothing                = 0
    | Map.findKey newpoint b == stone                  = 1
    | Map.findKey newpoint b == getOppositeStone stone = 0
    | otherwise                                        = countOneDirection game newpoint (xx, yy) stone
    where
        newpoint = Point (x + xx) (y + yy)



        
    
