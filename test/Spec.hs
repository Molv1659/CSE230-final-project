import GoLibrary

main :: IO ()
main = putStrLn $ "KoCheck: " ++ (show koCheckResult) ++ ",  " ++ 
                "SelfCaptureCheck1: " ++ (show selfCaptureCheck1Result) ++ ",  " ++ 
                "SelfCaptureCheck2: " ++ (show selfCaptureCheck2Result)

runHistory :: Game -> [Move] -> Game
runHistory game []                        = game
runHistory game (m@(Move point stone):ms) = runHistory (runMove game point stone) ms
runHistory game (m@(Pass stone):ms)       = runHistory (runPass game stone) ms

-- cannot put stone on Ko
koCheckResult :: Bool
koCheckResult = res 
    where
        res = (output == "Please put the stone on empty place.")
        output = isValidMove new_game (Point 3 6) White
        new_game = runHistory game koCheck
        game = createGo White 19
        koCheck = [ Move (Point 3 5) Black,
                    Move (Point 3 6) White,
                    Move (Point 2 6) Black,
                    Move (Point 2 7) White,
                    Move (Point 4 6) Black,
                    Move (Point 4 7) White,
                    Move (Point 1 1) Black,
                    Move (Point 3 8) White,
                    Move (Point 3 7) Black
                    ]

-- cannot self capture
selfCaptureCheck1Result :: Bool
selfCaptureCheck1Result = res 
    where
        res = (output == "You cannot put stone here. You're turning your stones into prisoners!")
        output = isValidMove new_game (Point 1 1) Black
        new_game = runHistory game selfCaptureCheck1
        game = createGo Black 19
        selfCaptureCheck1 = [   Move (Point 9 9) Black,
                                Move (Point 2 1) White,
                                Move (Point 9 8) Black,
                                Move (Point 2 2) White,
                                Move (Point 9 7) Black,
                                Move (Point 1 2) White,
                                Move (Point 9 6) Black,
                                Move (Point 5 5) White
                                ]

-- cannot self capture, unless you also capture your enemy
selfCaptureCheck2Result :: Bool
selfCaptureCheck2Result = res 
    where
        res = (output == "True")
        output = isValidMove new_game (Point 1 1) Black
        new_game = runHistory game selfCaptureCheck2
        game = createGo Black 19
        selfCaptureCheck2 = [   Move (Point 3 1) Black,
                                Move (Point 2 1) White,
                                Move (Point 3 2) Black,
                                Move (Point 2 2) White,
                                Move (Point 3 3) Black,
                                Move (Point 1 2) White,
                                Move (Point 2 3) Black,
                                Move (Point 5 5) White,
                                Move (Point 1 3) Black,
                                Move (Point 6 6) White
                                ]




