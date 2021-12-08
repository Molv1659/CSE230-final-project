module Networks(
    requestHandler,
    ) where
import           Data.ByteString.Char8     as D
import           Lens.Micro
import           Network.Socket
import           Network.Socket.ByteString


data EventType = LISTEN | CONNECT | SENDDATA | RECVDATA | DISCONNECT deriving (Eq, Ord)

data Point = Point {_i :: Int, _j :: Int}
                deriving (Ord, Eq)

data NetworkRequest = NetworkRequest {
    _eventType     :: EventType,
    _requestSocket :: Maybe Socket,
    _action        :: Either Point String
}

data NetworkResponse = NetworkResponse {
    _result         :: Bool,
    _responseSocket :: Maybe Socket,
    _msg            :: String
}

requestHandler :: NetworkRequest -> IO NetworkResponse
requestHandler (NetworkRequest etype socket (Right action)) = case etype of
    LISTEN -> lisenHandler
    CONNECT -> connectHandler action
    SENDDATA -> case socket of
                Just s -> sendDataHandler s action
                Nothing -> return $ NetworkResponse False (error "No socket") "No socket"
    RECVDATA -> case socket of
                Just s -> recvDataHandler s action
                Nothing -> return $ NetworkResponse False (error "No socket") "No socket"
    DISCONNECT -> case socket of
                Just s -> disconnectHandler s action
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
    socket <- startClient addr
    return $ NetworkResponse True (Just socket) "Connected"


sendData :: Socket -> String -> IO ()
sendData sock msg = do
    send sock (pack msg)
    return ()

sendDataHandler :: Socket -> String -> IO NetworkResponse
sendDataHandler sock msg = do
    sendData sock msg
    return $ NetworkResponse True (Just sock) "Sent"

recvDataHandler :: Socket -> String -> IO NetworkResponse
recvDataHandler sock msg = do
    recv sock 2048 >>= \x -> return $ NetworkResponse True (Just sock) (unpack x)

disconnectHandler :: Socket -> String -> IO NetworkResponse
disconnectHandler sock msg = do
    close sock
    return $ NetworkResponse True (Just sock) "Disconnected"


