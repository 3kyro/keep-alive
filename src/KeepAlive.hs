{-# LANGUAGE CPP,  ForeignFunctionInterface #-}
module KeepAlive
    ( KeepAlive (..)
    , KeepAliveError (..)
    , setKeepAlive
    , getKeepAliveOnOff
    ) where

import           Foreign
import           Foreign.C
import           LibForeign
import Data.Word (Word32)

-- The number of unacknowledged probes before considering the connection dead is platform specific.
-- See also : https://tldp.org/HOWTO/html_single/TCP-Keepalive-HOWTO/

-- | The main data structure definig keep alive parameters
data KeepAlive = KeepAlive
    { keepOnOff :: Bool
      -- ^ Turns on / off keep alive probes
    , keepIdle  :: Word32
      -- ^ The interval in seconds between the last data packet sent
      -- and the first keepalive probe.
    , keepIntvl :: Word32
      -- ^ The interval in seconds between subsequential keepalive probes.
    }
    deriving (Show, Eq, Ord)

-- |
data KeepAliveError
    = WSA_IO_PENDING
      -- ^ An overlapped operation was successfully initiated
      -- and completion will be indicated at a later time.
    | WSA_OPERATION_ABORTED
      -- ^ An overlapped operation was canceled due to the
      -- closure of the socket or the execution of the SIO_FLUSH IOCTL command.
    | WSAEFAULT
      -- ^ The lpOverlapped or lpCompletionRoutine parameter is not totally
      -- contained in a valid part of the user address space.
    | WSAEINPROGRESS
      -- ^ The function is invoked when a callback is in progress.
    | WSAEINTR
      -- ^ A blocking operation was interrupted.
    | WSAEINVAL
      -- ^ The dwIoControlCode parameter is not a valid command, or a specified
      -- input parameter is not acceptable, or the command is not applicable to
      -- the type of socket specified.
    | WSAENETDOWN
      -- ^ The network subsystem has failed.
    | WSAENOPROTOOPT
      -- ^ The socket option is not supported on the specified protocol.
      -- This error is returned for a datagram socket.
    | WSAENOTSOCK
      -- ^ The descriptor s is not a socket.
    | WSAEOPNOTSUPP
      -- ^ The specified IOCTL command is not supported. This error is returned if
      --the SIO_KEEPALIVE_VALS IOCTL is not supported by the transport provider.
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

