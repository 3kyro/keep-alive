{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- This module allows you to set per-connection keep alive parameters on windows and linux enviroments.
-- For more information on keep alive signals see https://en.wikipedia.org/wiki/Keepalive.
-- See also https://tldp.org/HOWTO/html_single/TCP-Keepalive-HOWTO/ for a linux specific implementation.
--
-- The module is meant to be used in conjuction with the "network" package. Speciffically, all functions require
-- a socket file descriptor argument which can be obtained by the withFdSocket function:
--
-- > -- sock is a Socket type
-- > withFdSocket sock $ \fd do
-- >    before <- getKeepAliveOnOff fd
-- >    print before -- False
-- >    -- set keep alive on, idle 60 seconds, interval 2 seconds
-- >    rlt <- setKeepAlive fd $ KeepAlive True 60 2
-- >    case rlt of
-- >        Left err -> print err
-- >        Right () -> return ()
-- >    after <- getKeepAliveOnOff fd
-- >    print after -- True

module Network.Socket.KeepAlive
    ( KeepAlive (..)
    , KeepAliveError (..)
    , setKeepAlive
    , getKeepAliveOnOff
    ) where

import           Data.Word  (Word32)
import           Foreign
import           Foreign.C
import           LibForeign


-- | The main data structure defining keep alive parameters
data KeepAlive = KeepAlive
    { kaOnOff   :: Bool
    -- ^ Turns on / off keep alive probes
    , kaIdle    :: Word32
    -- ^ The interval in seconds between the last data packet sent and the first keep alive probe
    , kaIntvl   :: Word32
    -- ^ The interval in seconds between subsequential keepalive probes
    }
    deriving (Show, Eq, Ord)

-- | Errors starting with WSA are windows specific
data KeepAliveError = WSA_IO_PENDING
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
    | EDOM
    | EINVAL
    | EISCONN
    | ENOPROTOOPT
    | ENOTSOCK
    | ENOMEM
    | ENOBUFS
    | OTHER_KEEPALIVE_ERROR
    deriving (Show, Eq, Ord)

-- | Set keep alive parameters for the current socket
setKeepAlive ::
    CInt
    -- ^ Socket file descriptor
    -> KeepAlive
    -- ^ Keep alive parameters
    -> IO ( Either KeepAliveError ())
setKeepAlive fd (KeepAlive onoff idle intvl) = do
    rlt <- setKeepAlive_ fd (cFromBool onoff) idle intvl
    return $ case rlt of
        0     -> Right ()
        997   -> Left WSA_IO_PENDING
        995   -> Left WSA_OPERATION_ABORTED
        10014 -> Left WSAEFAULT
        10036 -> Left WSAEINPROGRESS
        10004 -> Left WSAEINTR
        10022 -> Left WSAEINVAL
        10050 -> Left WSAENETDOWN
        10042 -> Left WSAENOPROTOOPT
        10038 -> Left WSAENOTSOCK
        10045 -> Left WSAEOPNOTSUPP
        9     -> Left EBADF
        33    -> Left EDOM
        22    -> Left EINVAL
        106   -> Left EISCONN
        92    -> Left ENOPROTOOPT
        88    -> Left ENOTSOCK
        12    -> Left ENOMEM
        105   -> Left ENOBUFS
        _     -> Left OTHER_KEEPALIVE_ERROR

-- | Returns True if keep alive is active for the specified socket
getKeepAliveOnOff ::
    CInt
    -- ^ Socket file descriptor
    -> IO Bool
getKeepAliveOnOff fd =
    cToBool . fromInteger . toInteger <$> getKeepAliveOnOff_ fd

cToBool :: Int -> Bool
cToBool x
    | x == 0 = False
    | otherwise = True

cFromBool :: Bool -> Word32
cFromBool True  = 1
cFromBool False = 0

