module Main where

import Network.Socket.KeepAlive
import qualified Network.Socket             as S
import           Data.IP.Internal           (IPv4)
import Data.IP (toHostAddress)
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import Foreign (sizeOf, void, with)

main :: IO ()
main = ffi

ffi :: IO()
ffi = do
    let ip = read "192.168.1.35"
    let port = 5502
    let addr = getAddr ip port
    withSocket addr $ \s -> do
        S.withFdSocket s $ \fd -> do
            before <- getKeepAliveOnOff fd
            print before
            rlt <- setKeepAlive fd test
            case rlt of
                Left err -> print err
                Right () -> return ()
            after <- getKeepAliveOnOff fd
            print after
        threadDelay 10000000000    
        return ()

test :: KeepAlive
test = KeepAlive True 10 15

withSocket :: S.SockAddr -> (S.Socket -> IO a) -> IO a
withSocket addr = bracket (connectTCP addr) close
  where close s = S.gracefulClose s 1000

connectTCP :: S.SockAddr -> IO S.Socket
connectTCP addr = do
    putStrLn ("Connecting to " ++ show addr ++ "...")
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect s addr
    putStrLn "connected"
    return s

getAddr :: IPv4 -> Int -> S.SockAddr
getAddr ip portNum = S.SockAddrInet (fromIntegral portNum) (toHostAddress ip)