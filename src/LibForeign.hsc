{-# LANGUAGE CPP,  ForeignFunctionInterface #-}
module LibForeign where

import Foreign
import Foreign.C.Types
import Foreign.C.Error
import Data.Word (Word32)

-- Platform specific includes
#ifdef _WIN32

# include "HsNet.h"

#else

#include <netinet/tcp.h>

#endif


getKeepAliveOnOff_ :: CInt -> IO CInt
getKeepAliveOnOff_ fd =
    alloca $ \ptr -> do
            let sz = fromIntegral $ sizeOf ( undefined :: CInt)
            with sz $ \ptr_sz -> do
                c_getsockopt fd c_SOL_SOCKET c_SO_KEEPALIVE ptr ptr_sz
                peek ptr

#ifdef _WIN32

setKeepAlive_ :: CInt -> Word32 -> Word32 -> Word32 -> IO CInt
setKeepAlive_ fd onoff idle intvl =
    c_winSetKeepAlive fd (fromIntegral onoff) (fromIntegral $ idle * 1000) (fromIntegral $ intvl * 1000)

#else

setKeepAlive_ :: CInt -> Word32 -> Word32 -> Word32 -> IO CInt
setKeepAlive_ fd onoff idle intvl = do

    let intOnOff = fromInteger $ toInteger onoff
    let intIdle = fromInteger $ toInteger idle
    let intIntvl = fromInteger $ toInteger intvl

    onoffrtn <- setKeepAliveOption_ fd c_SOL_SOCKET c_SO_KEEPALIVE intOnOff
    idlertn <- setKeepAliveOption_ fd c_SOL_TCP c_TCP_KEEPIDLE intIdle
    intrtn <- setKeepAliveOption_ fd c_SOL_TCP c_TCP_KEEPINTVL intIntvl

    -- Error check
    Errno rtn <-
        if onoffrtn + idlertn + intrtn /= 0
        then getErrno
        else return $ Errno 0
    return rtn

getKeepAliveOption_ :: CInt -> CInt -> CInt -> IO Int
getKeepAliveOption_ fd level option =
    alloca $ \ptr -> do
        let sz = fromIntegral $ sizeOf ( undefined :: CInt)
        with sz $ \ptr_sz -> do
            c_getsockopt fd level option ptr ptr_sz
            peek ptr

setKeepAliveOption_ :: CInt -> CInt -> CInt -> CInt -> IO CInt
setKeepAliveOption_ fd level option value = do
    let sz = fromIntegral $ sizeOf ( undefined :: Int)
    with value $ \ptr ->
        c_setsockopt fd level option ptr sz


#endif

c_SOL_SOCKET_ = #const SOL_SOCKET

c_SOL_SOCKET :: CInt
c_SOL_SOCKET = fromIntegral c_SOL_SOCKET_

c_SO_KEEPALIVE_ = #const SO_KEEPALIVE
 
c_SO_KEEPALIVE :: CInt
c_SO_KEEPALIVE = fromIntegral c_SO_KEEPALIVE_

#ifdef _WIN32

foreign import ccall unsafe "winSetKeepAlive"  
    c_winSetKeepAlive :: CInt -> CULong -> CULong -> CULong -> IO CInt         

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

