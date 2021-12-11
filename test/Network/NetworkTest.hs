import Network.Socket
import Networks
import NetworkInterface
import Control.Concurrent
import System.Environment
main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then
        putStrLn "Usage: add argument server or client to test network by adding '--test-arguments args'"
    else if head args == "server" then
        servertest
    else if head args == "client" then
        clienttest
    else
        putStrLn "Usage: add argument server or client to test network by adding '--test-arguments args'"

servertest :: IO ()
servertest = do
    putStrLn "test server"
    listentestRes <- listentest
    putStrLn ("CONNECT test:" ++ show (getResponseStatus listentestRes))
    recvRes <- recvtest $ getResponseSocket listentestRes
    putStrLn ("RECV test:" ++ show (getResponseMessage recvRes == "Test!"))

clienttest :: IO ()
clienttest = do
    putStrLn "test client"
    connecttestRes <- connecttest "127.0.0.1"
    putStrLn ("CONNECT test:" ++ show (getResponseStatus connecttestRes))
    sendRes <- sendtest "Test!" $ getResponseSocket connecttestRes
    putStrLn ("SEND test:" ++ show (getResponseStatus sendRes))


listentest :: IO NetworkResponse
listentest = do
    let request = NetworkRequest {
        _eventType = LISTEN,
        _requestSocket = Nothing,
        _action = Right ""
    }
    requestHandler request

connecttest :: String -> IO NetworkResponse
connecttest ip = do
    let request = NetworkRequest {
        _eventType = CONNECT,
        _requestSocket = Nothing,
        _action = Right ip
    }
    requestHandler request

sendtest :: String -> Socket -> IO NetworkResponse
sendtest msg s = do
    let request = NetworkRequest {
        _eventType = SENDDATA,
        _requestSocket = Just s,
        _action = Right msg
    }
    requestHandler request

recvtest :: Socket -> IO NetworkResponse
recvtest s = do
    let request = NetworkRequest {
        _eventType = RECVDATA,
        _requestSocket = Just s,
        _action = Right ""
    }
    requestHandler request
