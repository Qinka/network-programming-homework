
\subsection{Scan IP (Internal)}
\label{sec:scapip:internal}



\begin{code}
module Network.Netutils.ScanIP.Internal
       ( scanIP
       ) where

import Data.Maybe
import Data.Word
import Data.Bits
import Network.Netutils.Parsers
import Network.Netutils.Socket
import System.Timeout

import Data.ByteString.Builder(toLazyByteString,byteString,word16BE)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as Builder
\end{code}

If we want to know whether a remote host is online or offline, the normal way to find out answer is sending ICMP package.
By sending ICMP package, we can know whether a remote is online or offline and the know the delay between the two host, when the remote does not
ignore the ICMP package.

So what are needed are ip and the limit for timeout.
The following method will send a simple ICMP package, and try to receive from the remote. When it time out, method will return a \lstinline|Nothing|.
If it receive the message of the ICMP's response successfully, that means the the remote is online.
\begin{code}
scanIP :: Int -> (Word8,Word8,Word8,Word8) -> IO (Maybe [Word8])
scanIP to ip = do
  s <- socket :: IO (Socket Inet Raw ICMP)
  let x = toBS $ mkICMP 0 0
  sendTo s x mempty (SocketAddressInet (inetAddressFromTuple ip) 1)
  timeout to $ B.unpack <$> receive s 600 mempty
  where toBS ws = BL.toStrict $ toLazyByteString $ Prelude.foldr (\i b -> word16BE i `mappend` b) mempty ws
\end{code}
To send the ICMP package, there are a method to check out the check-sum, and a method to make up a ICMP package information.
\begin{code}
checkSum :: [Word16] -> Word16
checkSum is' =
  let is = Prelude.map fromIntegral is' :: [Word32]
      ss = sum is
      lo = fromIntegral ss
      hi = fromIntegral (ss `shiftR` 16)
  in lo + hi `xor` maxBound
mkICMP :: Word16 -> Word16 -> [Word16] 
mkICMP idt no = let ty = 0x800
                    csum = checkSum [ty,idt,no]
                in [ty,csum,idt,no]
\end{code}
