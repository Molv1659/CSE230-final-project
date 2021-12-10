module Networks(
    requestHandler,
    getResponseMessage,
    getResponseStatus
    ) where
    
import Prelude
import qualified Control.Exception         as E
import           Data.ByteString.Char8     as D (
    pack, unpack
    )
import           GoLibrary
import           Lens.Micro
import           Network.Socket
import           Network.Socket.ByteString
import           NetworkInterface
import Text.Read (
    readMaybe
    )

requestHandler :: NetworkRequest -> IO NetworkResponse
requestHandler (NetworkRequest etype sock act) = case etype of
    LISTEN -> listenHandler
    CONNECT -> connectHandler ac
                where
                    ac = case act of
                        Left _ -> error "should not happen"
                        Right msg  -> msg
    SENDDATA -> 
        case sock of
            Nothing -> return $ NetworkResponse False Nothing (Right "No socket, please connect or listen to connect to opponent")
            Just real_sock -> 
                case act of
                    Left p -> sendDataHandler real_sock (show p)
                    Right _ -> return NetworkResponse{_result=False, _responseSocket=Just real_sock, _msg=Right "Point type expected for SENDDATA"}
    RECVDATA -> 
        case sock of
            Just real_sock -> recvDataHandler real_sock
            Nothing -> return $ NetworkResponse{_result=False, _responseSocket=Nothing, _msg=Right "No socket"}
    DISCONNECT -> 
        case sock of
            Just real_sock -> disconnectHandler real_sock
            Nothing -> return $ NetworkResponse{_result=False, _responseSocket=Nothing, _msg=Right "No socket"}

listenHandler :: IO NetworkResponse
listenHandler = do
    (sock, addr) <- startServer
    return $ NetworkResponse True (Just sock) $ (Right $ "Accepted connection from " ++ show addr)


startServer :: IO (Socket, SockAddr)
startServer = do
    addr:_ <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) (Just "0.0.0.0") (Just "8080")
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 1
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
    r <- E.try (startClient addr) :: IO (Either E.SomeException Socket)
    case r of
        Left e -> return $ NetworkResponse {
            _result=False, 
            _responseSocket=Nothing, 
            _msg=Right $ show e
            }
        Right sock -> return $ NetworkResponse {
            _result=True, 
            _responseSocket=Just $ sock, 
            _msg=Right $ "Connected to " ++ show addr
            }

sendData :: Socket -> String -> IO ()
sendData sock msg = do
    _ <- send sock (pack msg)
    return ()

sendDataHandler :: Socket -> String -> IO NetworkResponse
sendDataHandler sock msg = do
    r <- E.try (sendData sock msg) :: IO (Either E.SomeException ())
    case r of
        Left e  -> return $ NetworkResponse{_result=False, _responseSocket=Just sock, _msg = Right $ show e}
        Right _ -> return $ NetworkResponse{_result=True, _responseSocket=Just sock, _msg = Right "Send data success"}

deserialize :: String -> Either Point String
deserialize point_string = case (readMaybe point_string:: Maybe Point) of
    Just p  -> Left p
    Nothing -> Right $ "Point parser error on for unpacked string: " ++ point_string ++ ", length: " ++ (show (length point_string))

recvDataHandler :: Socket -> IO NetworkResponse
recvDataHandler sock = do
    recv sock 2048 >>= \x -> 
        let point_or_str = deserialize $ unpack x
        in return $ NetworkResponse{
            _result=True,
            _responseSocket=Just sock,
            _msg=point_or_str}

disconnectHandler :: Socket -> IO NetworkResponse
disconnectHandler sock = do
    close sock
    return $ NetworkResponse True (Just sock) $ Right "Disconnected"

getResponseMessage :: NetworkResponse -> String
getResponseMessage (NetworkResponse _ _ (Right m)) = m
getResponseMessage (NetworkResponse _ _ (Left m)) = ""

getResponseStatus :: NetworkResponse -> Bool
getResponseStatus (NetworkResponse status _ _) = status
