{-# LANGUAGE CPP,  ForeignFunctionInterface #-}
module LibForeign where

import Foreign
import Foreign.C.Types
import Data.Word (Word32)

#ifdef _WIN32

# include <winsock2.h>
# include <ws2tcpip.h>
# include <mswsock.h>
# include <Mstcpip.h>
# include "HsNet.h"

#else

-- Check them on linux builds

-- #include <stdio.h>
-- #include <stdlib.h>
-- #include <unistd.h>
-- #include <sys/types.h>
-- #include <linux/socket.h>
-- #include <netinet/in.h>
-- #include <netinet/tcp.h>

#endif



#ifdef _WIN32

setKeepAlive_ :: CInt -> Word32 -> Word32 -> Word32 -> IO ()
setKeepAlive_ fd onoff idle intvl = 
    c_winSetKeepAlive fd (fromIntegral onoff) (fromIntegral $ idle * 1000) (fromIntegral $ intvl * 1000)

#else

#endif

c_SOL_SOCKET_ = #const SOL_SOCKET

c_SOL_SOCKET :: CInt
c_SOL_SOCKET = fromIntegral c_SOL_SOCKET_

c_SO_KEEPALIVE_ = #const SO_KEEPALIVE
 
c_SO_KEEPALIVE :: CInt
c_SO_KEEPALIVE = fromIntegral c_SO_KEEPALIVE_

#ifdef _WIN32

foreign import ccall unsafe "winSetKeepAlive"  
    c_winSetKeepAlive :: CInt -> CULong -> CULong -> CULong -> IO ()         

#else

c_SOL_TCP_ = #const SOL_TCP

c_SOL_TCP :: CInt
c_SOL_TCP = fromIntegral c_SOL_TCP_

c_TCP_KEEPIDLE_ = #const TCP_KEEPIDLE

c_TCP_KEEPIDLE :: CInt
c_TCP_KEEPIDLE = fromIntegral c_TCP_KEEPIDLE_ 


c_TCP_KEEPCNT_ = #const TCP_KEEPCNT 

c_TCP_KEEPCNT :: CInt
c_TCP_KEEPCNT = fromIntegral c_TCP_KEEPCNT_  

c_TCP_KEEPINTVL_ = #const TCP_KEEPINTVL

c_TCP_KEEPINTVL :: CInt 
c_TCP_KEEPINTVL = fromIntegral c_TCP_KEEPINTVL_ 

#endif

foreign import ccall unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt   
foreign import ccall unsafe "setsockopt"   
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt  
 


