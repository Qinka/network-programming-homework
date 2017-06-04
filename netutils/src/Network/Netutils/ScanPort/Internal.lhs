

\begin{code}
module Network.Netutils.ScanPort.Internal
       ( scanPortTCP
       , scanPortUDP
       , mkInetAddr
       , mkInet6Addr
       ) where

import Data.Maybe
import Data.Word
import Data.Bits
import Network.Netutils.Parsers
import Network.Netutils.Socket
import System.Timeout
import Control.Exception
import Control.Concurrent

import Data.ByteString.Builder(toLazyByteString,byteString,word16BE)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as Builder
\end{code}

\begin{code}
scanPortTCP :: (HasAddressInfo f,Show (SocketAddress f)) => Int -> SocketAddress f -> IO (Bool,Socket f Stream TCP)
scanPortTCP to addr = do
  print addr
  sock <- socket
  rt <- timeout to $ try $ connect sock addr :: IO (Maybe (Either SocketException ()))
  close sock
  return $ case rt of
    Just (Right _) -> (True,sock)
    _ -> (False,sock)
\end{code}


\begin{code}
scanPortUDP :: (HasAddressInfo f,Show (SocketAddress f)) => Int -> SocketAddress f -> IO (Bool,Socket f Datagram UDP)
scanPortUDP to addr = do
  print addr
  sock <- socket
  rt <- try $ cnn sock addr :: IO (Either SocketException ())
  close sock
  return $ case rt of
    Left _ -> (False,sock)
    Right _ -> (True,sock)
  where cnn sock addr = do
          connect sock addr
          send sock "" mempty
          timeout to (receive sock 1 mempty)
          close sock
\end{code}

\begin{code}
mkInetAddr :: (Word8,Word8,Word8,Word8) -> Word16 -> SocketAddress Inet
mkInetAddr ip port = SocketAddressInet (inetAddressFromTuple ip) (fromIntegral port)
mkInet6Addr :: (Word16,Word16,Word16,Word16,Word16,Word16,Word16,Word16)
            -> Word16 -> Word32 -> Word32 -> SocketAddress Inet6
mkInet6Addr ip port sid fi = SocketAddressInet6
  (inet6AddressFromTuple ip) (fromIntegral port)
  (fromIntegral fi) (fromIntegral sid)
\end{code}
