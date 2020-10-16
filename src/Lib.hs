{-# LANGUAGE CPP,  ForeignFunctionInterface #-}
module Lib 
    ( KeepAlive (..)
    , setKeepAlive
    , getKeepAliveOnOff
    ) where

import           Foreign
import           Foreign.C
import           LibForeign
import Data.Word (Word32)


data KeepAlive = KeepAlive
    { keepOnOff :: Bool
    , keepIdle  :: Word32
    , keepIntvl :: Word32
    }
    deriving (Show, Eq, Ord)

data KeepAliveError
    = WSA_IO_PENDING
    | WSA_OPERATION_ABORTED
    | WSAEFAULT
    | WSAEINPROGRESS
    | WSAEINTR
    | WSAEINVAL
    | WSAENETDOWN
    | WSAENOPROTOOPT
    | WSAENOTSOCK
    | WSAEOPNOTSUPP
    | OTHERKEEPALIVEERROR
    deriving (Show, Eq, Ord)


setKeepAlive :: CInt -> KeepAlive -> IO ( Either KeepAliveError ())
setKeepAlive fd (KeepAlive onoff idle intvl) = do
    rlt <- setKeepAlive_ fd (cFromBool onoff) idle intvl
    return $ case rlt of
        0 -> Right ()
        997 -> Left WSA_IO_PENDING
        995 -> Left WSA_OPERATION_ABORTED
        10014 -> Left WSAEFAULT
        10036 -> Left WSAEINPROGRESS
        10004 -> Left WSAEINTR 
        10022 -> Left WSAEINVAL
        10050 -> Left WSAENETDOWN
        10042 -> Left WSAENOPROTOOPT
        10038 -> Left WSAENOTSOCK
        10045 -> Left WSAEOPNOTSUPP
        _ -> Left OTHERKEEPALIVEERROR

getKeepAliveOnOff :: CInt -> IO Bool
getKeepAliveOnOff fd = 
    cToBool . fromInteger . toInteger <$> getKeepAliveOnOff_ fd
 
cToBool :: Int -> Bool
cToBool x
    | x == 0 = False
    | otherwise = True

cFromBool :: Bool -> Word32
cFromBool True = 1
cFromBool False = 0

