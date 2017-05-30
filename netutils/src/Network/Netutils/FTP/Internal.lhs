
\subsection{FTP Internal Communication}
\label{sec:ftp:internal}
\begin{code}
module Network.Netutils.FTP.Internal
       (
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

\begin{code}
connectFTP :: forall f.HasAddressInfo f
           => B.ByteString
           -> IO (Socket f Stream TCP)
connectFTP ftpUrl = do
  hSock <- socket
  case parsingURL ftpURL of
    Left err -> error $ BC.unpack err
    Right URL{..} -> do
      let realPort = fromMaybe urlProtocal urlPort
      addr <-  head <$> getAddressInfo (Just urlHostName)
        (Just realPort) mempty
      connect hSock $ socketAddress addr
      return hSock
\end{code}

\begin{code}
closeFTP :: Socket f Stream TCP -> IO ()
closeFTP = close
\end{code}

\begin{code}
authFTP :: Socket f Stream TCP
        -> B.ByteString -> B.ByteString
        -> IO (Bool,B.ByteString)
authFTP hSock user pass = do
  sendAllBuilder hSock 1024 (userBuilder "" "\r\n") mempty
  rt1 <- readUntilEndLine sock mempty
  if getFTPLOGINC rt1
    then do
    sendAllBuilder hSock 1024 (passBuilder "" "\r\n") mempty
    rt2 <- readUntilEndLine sock mempty
    return (getFTPLOGINS rt2,BL.toStrict $ toLazyByteString $ foldr mappend mempty
             [ userBuilder ">> " "\n"
             , byteString rt1
             , passBuilder "\n>> " "\n"
             , byteString rt2
             ])
    else do
    return (False,BL.toStrict $ toLazyByteString $ foldr mappend mempty
             [ userBuilder ">> " "\n"
             , byteString rt1
             ])
  where userBuilder prefix postfix = foldr mappend mempty
          [ byteString prefix
          , byteString "USER "
          , byteString user
          , byteString postfix
          ]
        passBuilder prefix postfix = foldr mappend mempty
          [ byteString prefix
          , byteString "PASS "
          , byteString pass
          , byteString postfix
          ]          
\end{code}

\begin{code}
pasvFTP :: Socket f Stream TCP -> IO (Just (Socket f Stream TCP),B.ByteString)
pasvFTP hSock = do
  sendAllBuilder hSock 1024 (pasvBuilder "" "\r\n")
  rt <- readUntilEndLine hSock mempty
  pS <- case getFTPPASV rt of
          Just (host,port) -> do
            x <- head <$> getAddressInfo (Just host) (Just port)
            pSock <- socker
            connect pSock
            return $ Just pSock
          Nothing -> return Nothing
  return (pS,BL.toStrict $ toLazyByteString $ foldr mappend mempt
             [ pasvBuilder ">> " "\n"
             , byteString rt
             ])
  where pasvBuilder prefix postfix = foldr mappend mempty
          [ byteString prefix
          , byteString "PASV"
          , byteString postfix
          ]
\end{code}
