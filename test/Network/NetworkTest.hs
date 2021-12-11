import Networks
import NetworkInterface
import Control.Concurrent
import System.Environment
main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then
        putStrLn "Usage: add argument server or client to test network by adding '--test-arguments args'"
    else if args !! 0 == "server" then
        servertest
    else if args !! 0 == "client" then
        clienttest
    else
        putStrLn $ "Usage: add argument server or client to test network by adding '--test-arguments args'"

servertest :: IO ()
servertest = do
    putStrLn "test server"
    listentestRes <- listentest
    putStrLn ("LISTEN test:" ++ show listentestRes)

clienttest :: IO ()
clienttest = do
    putStrLn "test client"
    connecttestRes <- connecttest "127.0.0.1"
    putStrLn ("CONNECT test:" ++ show connecttestRes)

listentest :: IO Bool
listentest = do
    let request = NetworkRequest {
        _eventType = LISTEN,
        _requestSocket = Nothing,
        _action = Right ""
    }
    response <- requestHandler request
    return $ getResponseStatus response

connecttest :: String -> IO Bool
connecttest ip = do
    let request = NetworkRequest {
        _eventType = CONNECT,
        _requestSocket = Nothing,
        _action = Right ip
    }
    response <- requestHandler request
    return $ getResponseStatus response
