module Networks(
    startServer,
    echoServer,
    clientPlayer,
    serverPlayer
    ) where
import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Run.TCP
{-
   startServer: Run a TCP server on port 8080
   It accept an IO action
-}
startServer :: (Socket -> IO a) -> IO a
startServer = runTCPServer (Just "0.0.0.0") "8080"

{-
   Example of a echo server
   You can use netcat to test it
-}
echoServer :: IO a
echoServer = startServer echo
    where
        echo s = do
            message <- recv s 4096
            putStrLn "Server: "
            C.putStrLn message
            sendAll s message
            echo s


{-
   startClient: Try to connect to a TCP server
   It accept an IO action after get the connection
-}
startClient addr = runTCPClient addr "8080"


{-
   Example of a client that want to join the game
-}
clientPlayer :: IO ()
clientPlayer = startClient "0.0.0.0" join
    where
        join s = do
            sendAll s (C.pack "Hello, I want to join the game!")
            message <- recv s 4096
            putStr "Server: "
            C.putStrLn message
            join s

handleJoin :: (Eq a, Num a) => a -> Socket -> IO ()
handleJoin num s = do
    message <- recv s 4096
    if C.unpack message == "Hello, I want to join the game!"
        then do
            if num == 0
                then do
                    sendAll s (C.pack "Welcome to the game!")
                    handleJoin (num + 1) s
                else do
                    sendAll s (C.pack "Sorry, the game is full, you can only be the observer")
    else do
        sendAll s (C.pack "Message error!")

serverPlayer :: IO ()
serverPlayer = do
    let num = 0
    startServer (handleJoin num)

