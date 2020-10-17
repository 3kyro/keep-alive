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
    | EBADF
      -- ^ The socket argument is not a valid file descriptor.
    | EDOM
      -- ^ The send and receive timeout values are too big to
      -- fit into the timeout fields in the socket structure.
    | EINVAL
      -- ^ The specified option is invalid at the specified socket level
      -- or the socket has been shut down.
    | EISCONN
      -- ^ The socket is already connected, and a specified option cannot
      -- be set while the socket is connected.
    | ENOPROTOOPT
      -- ^ The option is not supported by the protocol.
    | ENOTSOCK
      -- ^ The socket argument does not refer to a socket.
    | ENOMEM
      -- ^ There was insufficient memory available for the operation to complete.
    | ENOBUFS
      -- ^ Insufficient resources are available in the system to complete the call.
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
        9 -> Left EBADF
        33 -> Left EDOM
        22 -> Left EINVAL
        106 -> Left EISCONN
        92 -> Left ENOPROTOOPT
        88 -> Left ENOTSOCK
        12 -> Left ENOMEM
        105 -> Left ENOBUFS
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

