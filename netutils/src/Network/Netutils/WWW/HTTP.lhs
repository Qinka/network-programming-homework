
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

The way to send the HTTP's GET request is by using TCP socket.
There are four step to send a GET request.

The first step is connect the TCP with the HTTP server.
To connect it, we need to information: ip and port.
After the url given, the parser can get the hostname, protocol, and the port of the url.
Then by searching the name server, the real ip and the port can be found.
Finally, the connection can be establish.
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

And to make sure the socket connection can be closed, there is a method \lstinline|withHTTP| defined.
Such a method will take in another method close the socket after that method finished, and return the result.
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

When sending the request, the first step of all is sending the "GET" header of the request.
In this ``line'', there are informations about type of the request, version of the HTTP, and request path.
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

The other information followed the first line is about the headers of the HTTP.
One of the header is the host's hostname.
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
One of the header is the MIME type of what you want.
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
One of the header is about the users' agent.
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
After sending all the headers needed, an empty line, which means the end of the request, is needed to be sended.
\begin{code}
rqHTTPSendEnd :: Family f => Socket f Stream TCP -> IO B.ByteString
rqHTTPSendEnd hSock = do
  sendAllBuilder hSock 1024 (msgBuilder "" "\r\n") mempty
  return $ BL.toStrict $ toLazyByteString $ msgBuilder ">> " "\n"
  where msgBuilder prefix postfix = byteString prefix `mappend` byteString postfix
\end{code}
The final one high of the to-do list is the receive the response from the service.
All we need to do, firstly, is receive the response's headers, and when received an empty line, that means all headers we had received. Then the size of the body can be found. Finally, the body of the request will be received.
\begin{code}
rqHTTPReceive :: Family f => Socket f Stream TCP -> IO B.ByteString
rqHTTPReceive hSock = do
  hHead <- readUntilBlankLine hSock mempty
  let len = getBodyLength hHead
  hBody <- receive hSock len mempty
  return $ hHead `B.append` hBody
\end{code}

Then encapsulate methods into one.
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



