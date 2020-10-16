{-# LANGUAGE ForeignFunctionInterface #-}
module LibForeign where

import Foreign
import Foreign.C.Types

#include <sys/types.h>
#include <linux/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>

c_SOL_SOCKET_ = #const SOL_SOCKET

c_SOL_SOCKET :: CInt
c_SOL_SOCKET = fromIntegral c_SOL_SOCKET_

c_SO_KEEPALIVE_ = #const SO_KEEPALIVE

c_SO_KEEPALIVE :: CInt
c_SO_KEEPALIVE = fromIntegral c_SO_KEEPALIVE_

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






