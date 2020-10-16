module Lib
    (
    ) where

import           Foreign
import           Foreign.C
import           LibForeign

data KeepAliveOptions = KeepAliveOptions
    { keepAlive :: Bool
    , keepIdle  :: Int
    , keepCnt   :: Int
    , keepIntvl :: Int
    }
    deriving (Show)


getKeepAliveOption :: CInt -> IO KeepAliveOptions
getKeepAliveOption fd = do
    ka <- getKeepAliveOption_ fd c_SOL_SOCKET c_SO_KEEPALIVE
    kidle <- getKeepAliveOption_ fd c_SOL_TCP c_TCP_KEEPIDLE
    kcnt <- getKeepAliveOption_ fd c_SOL_TCP c_TCP_KEEPCNT
    kintv <- getKeepAliveOption_ fd c_SOL_TCP c_TCP_KEEPINTVL
    return $ KeepAliveOptions (cBool ka) kidle kcnt kintv


getKeepAliveOption_ :: CInt -> CInt -> CInt -> IO Int
getKeepAliveOption_ fd level option =
    alloca $ \ptr -> do
        let sz = fromIntegral $ sizeOf ( undefined :: CInt)
        with sz $ \ptr_sz -> do
            c_getsockopt fd level option ptr ptr_sz
            peek ptr

cBool :: Int -> Bool
cBool x
    | x == 0 = False
    | otherwise = True