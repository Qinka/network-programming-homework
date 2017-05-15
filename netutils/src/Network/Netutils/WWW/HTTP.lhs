
\subsection{HTTP Communication}
\label{sec:http:communication}
\begin{code}
module Network.Netutils.WWW.HTTP
       ( connectHTTP
       , withHTTP
       , rqHTTPSendVersion
       , rqHTTPSendHostName
       , rqHTTPSendAccept
       , rqHTTPSendUserAgent
       , rqHTTPSendEnd
       , rqHTTPReceve
       , doHTTP
       ) where

import Network.Netutils.URLParse

import System.Socket
import System.Socket.Family.Inet
import System.Socket.Family.Inet6
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream
import Data.ByteString.Builder(toLazyByteString,byteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as Builder
\end{code}


The method to connect the web site.
\begin{code}
connectHTTP :: Family f => B.ByteString -> IO (Socket f Stream TCP)
connectHTTP httpUrl = do
  hSock <- socket
  addr:_ <- getAddressInfo (Just httpUrl) (Just "http") mempty
  connect hSock $ socketAddress addr
  return hSock
\end{code}

The method work with the socket.
\begin{code}
withHTTP :: Family f => B.ByteString -> (Socket f Stream TCP -> IO B.ByteString) -> IO B.ByteString
withHTTP httpUrl func = do
  hs <- connectHTTP httpUrl
  func  hs
  close hs
\end{code}


\begin{code}
rqHTTPSendVersion :: Family f => Socket f Stream TCP -> B.ByteString -> IO B.ByteString
rqHTTPSendVersion hSock path = do
  sendAllBuilder hSock 1024 (msgBuilder "" "\n") mempty
  return $ toLazyByteString $ msgBuilder ">> " "\n"
  where msgBuilder prefix postfix = foldr mappend (byteString prefix)
          [ byteString "GET /"
          , byteString msg
          , byteString " HTTP/1.1"
          , byteString postfix
          ]
                       
\end{code}

\begin{code}
rqHTTPSendHostName :: Family f => Socket f Stream TCP -> B.ByteString -> IO B.ByteString
rqHTTPSendHostName hSock hostname = do
  sendAllBuilder hSock 1024 (msgBuilder "" "\n") mempty
  return $ toLazyByteString $ msgBuilder ">> " "\n"
  where msgBuilder prefix postfix = foldr mappend (byteString prefix)
          [ byteString "Host:"
          , byteString hostname
          , byteString postfix
          ]
\end{code}

\begin{code}
rqHTTPSendAccept :: Family f => Socket f Stream TCP -> IO B.ByteString
rqHTTPSendAccept hSock = do
  send hSock (toLazyByteString $ msgBuilder "" "\n") mempty
  return $ toLazyByteString $ msgBuilder ">> " "\n"
  where msgBuilder msg prefix postfix = foldr mappend (byteString prefix)
          [ byteString "Accept:text:/html"
          , byteString postsfix
          ]
\end{code}

\begin{code}
rqHTTPSendUserAgent :: Family f => Socket f Stream TCP -> B.ByteString -> IO B.ByteString
rqHTTPSendUserAgent hSock ua = do
  sendAllBuilder hSock 1024 (msgBuilder "" "\n") mempty
  return $ toLazyByteString $ msgBuilder ">> " "\n"
  where msgBuilder msg prefix postfix = foldr mappend (byteString prefix)
          [ byteString ua
          , bytrString postfix
          ]
\end{code}


\begin{code}
rqHTTPSendEnd :: Family f => Socket f Stream TCP -> IO B.ByteString
rqHTTPSendEnd hSock = do
  sendAllBuilder hSock 1024 (msgBuilder "" "\n") mempty
  return $ toLazyByteString $ msgBuilder ">> " "\n"
  where msgBuilder msg prefix postfix = byteString prefix `mappend` byteString postfix
\end{code}

\begin{code}
rqHTTPReceive :: Family f => Socket f Stream TCP -> IO B.ByteString
rqHTTPReceive hSock = receiveAll hSock 1048576 mempty
\end{code}


\begin{code}
doHTTPv4 :: B.ByteString -> IO B.ByteString
doHTTPv4 url = do
  let url' = parsingURL url
  case url' of
    Left 
\end{code}
