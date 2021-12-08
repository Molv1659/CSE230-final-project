{-# LANGUAGE TemplateHaskell #-}
module NetworkInterface where

import qualified Network.Socket as S
import Lens.Micro (
    over
    , (^.), (&), (.~), (%~)
    )
import Lens.Micro.TH (
    makeLenses
    )
import GoLibrary as Lib

data EventType = LISTEN | CONNECT | SENDDATA | RECVDATA | DISCONNECT deriving (Eq, Ord)

data NetworkRequest = NetworkRequest {
    _eventType :: EventType,
    _requestSocket :: Maybe S.Socket,
    _action :: Either Lib.Point String
}

makeLenses ''NetworkRequest

data NetworkResponse = NetworkResponse {
    _result :: Bool,
    _responseSocket :: Maybe S.Socket,
    _msg :: String
}

makeLenses ''NetworkResponse

