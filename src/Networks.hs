module Networks(
    requestHandler,
    getResponseMessage,
    getResponseStatus
    ) where
import qualified Control.Exception         as E
import           Data.ByteString.Char8     as D
import           GoLibrary
import           Lens.Micro
import           Network.Socket
import           Network.Socket.ByteString
import           NetworkInterface

requestHandler :: NetworkRequest -> IO NetworkResponse
requestHandler (NetworkRequest etype socket action) = case etype of
    LISTEN -> lisenHandler
    CONNECT -> connectHandler ac
                where
                    ac = case action of
                        Left point -> error "should not happen"
                        Right msg  -> msg
    SENDDATA -> case socket of
                Just s -> sendDataHandler s ac
                Nothing -> return $ NetworkResponse False (error "No socket") "No socket"
                where ac = case action of
                        Left point -> (show point)
                        Right msg  -> msg
    RECVDATA -> case socket of
                Just s -> recvDataHandler s
                Nothing -> return $ NetworkResponse False (error "No socket") "No socket"
    DISCONNECT -> case socket of
                Just s -> disconnectHandler s
                Nothing -> return $ NetworkResponse False (error "No socket") "No socket"

lisenHandler :: IO NetworkResponse
lisenHandler = do
    (socket, addr) <- startServer
    return $ NetworkResponse True (Just socket) $ "Accpet connection from " ++ show addr


startServer :: IO (Socket, SockAddr)
startServer = do
    addr:_ <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) (Just "0.0.0.0") (Just "8080")
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 1024
    accept sock

startClient :: String -> IO Socket
startClient addr = do
    conn:_ <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) (Just addr) (Just "8080")
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    connect sock (addrAddress conn)
    return sock

connectHandler :: String -> IO NetworkResponse
connectHandler addr = do
    result <- E.try (startClient addr) :: IO (Either E.SomeException Socket)
    case result of
        Left e -> return $ NetworkResponse False Nothing $ show e
        Right s -> return $ NetworkResponse True (Just s) $ "Connected to " ++ show addr


sendData :: Socket -> String -> IO ()
sendData sock msg = do
    send sock (pack msg)
    return ()

sendDataHandler :: Socket -> String -> IO NetworkResponse
sendDataHandler sock msg = do
    result <- E.try (sendData sock msg) :: IO (Either E.SomeException ())
    case result of
        Left e  -> return $ NetworkResponse False Nothing $ show e
        Right _ -> return $ NetworkResponse True Nothing "Send data success"

recvDataHandler :: Socket -> IO NetworkResponse
recvDataHandler sock = do
    recv sock 2048 >>= \x -> return $ NetworkResponse True (Just sock) (unpack x)

disconnectHandler :: Socket -> IO NetworkResponse
disconnectHandler sock = do
    close sock
    return $ NetworkResponse True (Just sock) "Disconnected"

getResponseMessage :: NetworkResponse -> String
getResponseMessage (NetworkResponse _ _ msg) = msg

getResponseStatus :: NetworkResponse -> Bool
getResponseStatus (NetworkResponse status _ _) = status
