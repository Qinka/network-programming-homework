
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
       , rqHTTPReceive
       , doHTTPv4
       ) where

import Data.Maybe
import Network.Netutils.Parsers
import Network.Netutils.Socket

import Data.ByteString.Builder(toLazyByteString,byteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as Builder
\end{code}


The method to connect the web site.
\begin{code}
connectHTTP :: forall f.HasAddressInfo f
            => B.ByteString
            -> IO (Socket f Stream TCP,B.ByteString,B.ByteString)
connectHTTP httpUrl = do
  hSock <- socket
  let url' = parsingURL httpUrl
  case url' of
    Left err -> error $ BC.unpack err
    Right URL{..} -> do
      let realPort = fromMaybe urlProtocal urlPort
      addr:_ <- getAddressInfo (Just urlHostName)
        (Just realPort) mempty :: HasAddressInfo f
                               => IO [AddressInfo f Stream TCP]
      connect hSock $ socketAddress addr
      return (hSock,urlHostName,urlPath)
\end{code}

The method work with the socket.
\begin{code}
withHTTP :: HasAddressInfo f
         => B.ByteString
         -> (Socket f Stream TCP  -> B.ByteString -> B.ByteString -> IO B.ByteString)
         -> IO B.ByteString
withHTTP httpUrl func = do
  (hs,uhn,up) <- connectHTTP httpUrl
  texts <- func hs uhn up
  close hs
  return texts
\end{code}


\begin{code}
rqHTTPSendVersion :: Family f => Socket f Stream TCP -> B.ByteString -> IO B.ByteString
rqHTTPSendVersion hSock path = do
  sendAllBuilder hSock 1024 (msgBuilder "" "\r\n") mempty
  return $ BL.toStrict $ toLazyByteString $ msgBuilder ">> " "\n"
  where msgBuilder prefix postfix = foldr mappend (byteString postfix)
          [ byteString prefix
          , byteString "GET "
          , byteString path
          , byteString " HTTP/1.0"
          ]                       
\end{code}

\begin{code}
rqHTTPSendHostName :: Family f => Socket f Stream TCP -> B.ByteString -> IO B.ByteString
rqHTTPSendHostName hSock hostname = do
  sendAllBuilder hSock 1024 (msgBuilder "" "\r\n") mempty
  return $ BL.toStrict $ toLazyByteString $ msgBuilder ">> " "\n"
  where msgBuilder prefix postfix = foldr mappend (byteString postfix)
          [ byteString prefix
          , byteString "Host:"
          , byteString hostname
          ]
\end{code}

\begin{code}
rqHTTPSendAccept :: Family f => Socket f Stream TCP -> IO B.ByteString
rqHTTPSendAccept hSock = do
  send hSock (BL.toStrict $ toLazyByteString $ msgBuilder "" "\n") mempty
  return $ BL.toStrict $ toLazyByteString $ msgBuilder ">> " "\n"
  where msgBuilder prefix postfix = foldr mappend (byteString postfix)
          [ byteString prefix
          , byteString "Accept:text:/html"
          ]
\end{code}

\begin{code}
rqHTTPSendUserAgent :: Family f => Socket f Stream TCP -> B.ByteString -> IO B.ByteString
rqHTTPSendUserAgent hSock ua = do
  sendAllBuilder hSock 1024 (msgBuilder "" "\r\n") mempty
  return $ BL.toStrict $ toLazyByteString $ msgBuilder ">> " "\n"
  where msgBuilder prefix postfix = foldr mappend (byteString postfix)
          [ byteString prefix
          , byteString "UserAgent:"
          , byteString ua
          ]
\end{code}


\begin{code}
rqHTTPSendEnd :: Family f => Socket f Stream TCP -> IO B.ByteString
rqHTTPSendEnd hSock = do
  sendAllBuilder hSock 1024 (msgBuilder "" "\r\n") mempty
  return $ BL.toStrict $ toLazyByteString $ msgBuilder ">> " "\n"
  where msgBuilder prefix postfix = byteString prefix `mappend` byteString postfix
\end{code}

\begin{code}
rqHTTPReceive :: Family f => Socket f Stream TCP -> IO B.ByteString
rqHTTPReceive hSock = do
  hHead <- readUntilBlankLine hSock mempty
  let len = getBodyLength hHead
  hBody <- receive hSock len mempty
  return $ hHead `B.append` hBody
\end{code}


\begin{code}
doHTTPv4 :: B.ByteString -> IO B.ByteString
doHTTPv4 url = withHTTP url worker
  where worker :: Socket Inet Stream TCP -> B.ByteString -> B.ByteString -> IO B.ByteString
        worker sock uhn up = do
          rsv  <- rqHTTPSendVersion   sock up
          rshn <- rqHTTPSendHostName  sock uhn
          rsa  <- rqHTTPSendAccept    sock
          rsua <- rqHTTPSendUserAgent sock "Mozilla/5.0  AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36 Edge/15.15063"
          rqse <- rqHTTPSendEnd       sock
          rqrc <- rqHTTPReceive       sock
          return $ BL.toStrict $ toLazyByteString $ foldr (\a b -> (byteString a) `mappend` b) mempty
            [rsv, rshn, rsa, rsua, rqse, rqrc]    
\end{code}



