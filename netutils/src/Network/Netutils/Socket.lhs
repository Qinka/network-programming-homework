\begin{code}
module Network.Netutils.Socket
       ( module Socket
       , ICMP(..)
       , readUntilDotLine
       , readUntilEndLine
       , readUntilBlankLine
       ) where

import System.Socket                  as Socket
import System.Socket.Family.Inet      as Socket
import System.Socket.Family.Inet6     as Socket
import System.Socket.Protocol.TCP     as Socket
import System.Socket.Type.Stream      as Socket
import System.Socket.Type.Raw         as Socket

import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
\end{code}


\begin{code}
readUntilDotLine :: Socket f Stream p -> MessageFlags -> IO B.ByteString
readUntilDotLine sock msgf = BL.toStrict . toLazyByteString <$> roll mempty "" "" "" ""
  where roll builder a b c d = do
          e <- receive sock 1 msgf
          if and (zipWith (==) [a,b,c,d,e] ["\r","\n",".","\r","\n"])
            then return $! builder `mappend` byteString (a `B.append` b `B.append` c `B.append` d `B.append` e)
            else builder `seq` roll (builder `mappend` byteString a) b c d e
\end{code}

\begin{code}
readUntilEndLine :: Socket f Stream p -> MessageFlags -> IO B.ByteString
readUntilEndLine sock msgf = BL.toStrict . toLazyByteString <$> roll mempty ""
  where roll builder a = do
          b <- receive sock 1 msgf
          if and (zipWith (==) [a,b] ["\r","\n"])
            then return $! builder `mappend` byteString (a `B.append` b)
            else builder `seq` roll (builder `mappend` byteString a) b
\end{code}


\begin{code}
readUntilBlankLine :: Socket f Stream p -> MessageFlags -> IO B.ByteString
readUntilBlankLine sock msgf = BL.toStrict . toLazyByteString <$> roll mempty ("","","")
  where roll builder (a,b,c) = do
          d <- receive sock 1 msgf
          if and (zipWith (==) [a,b,c,d] ["\r","\n","\r","\n"])
            then return $! builder `mappend` byteString (foldr B.append d [a,b,c])
            else builder `seq` roll (builder `mappend` byteString a) (b,c,d)
\end{code}

\begin{code}
data ICMP = ICMP
instance Protocol ICMP where
  protocolNumber _ = 1
\end{code}
