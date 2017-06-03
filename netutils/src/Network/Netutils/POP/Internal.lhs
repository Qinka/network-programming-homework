
\subsection{POP Internal Communication}
\label{sec:pop:internal}

\begin{code}
module Network.Netutils.POP.Internal
       ( connectPOP3
       , authPOP3
       , quitPOP3
       , statPOP3
       , retrPOP3
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
connectPOP3 :: forall f.HasAddressInfo f
            => B.ByteString
            -> IO (Socket f Stream TCP,B.ByteString)
connectPOP3 popUrl = do
  hSock <- socket
  case parsingURL popUrl of
    Left err -> error $ BC.unpack err
    Right URL{..} -> do
      let realPort = fromMaybe urlProtocal urlPort
      addr <- head <$> getAddressInfo (Just urlHostName)
        (Just realPort) mempty :: HasAddressInfo f
                               => IO (AddressInfo f Stream TCP)
      connect hSock $ socketAddress addr
      (_,str) <- readPOPR hSock mempty
      return (hSock,str)
\end{code}

\begin{code}
quitPOP3 :: Socket f Stream TCP -> IO (B.ByteString)
quitPOP3 hSock = do
  sendAllBuilder hSock 1024 (quitBuilder "" "\r\n") mempty
  (_,str) <- readPOPR hSock mempty
  return $ BL.toStrict $ toLazyByteString $ quitBuilder ">> " "\n" `mappend` byteString str
  where quitBuilder prefix postfix = foldr mappend mempty
          [ byteString prefix
          , byteString "QUIT"
          , byteString postfix
          ]
\end{code}

\begin{code}
authPOP3 :: Socket f Stream TCP
         -> B.ByteString -> B.ByteString
         -> IO (Bool,B.ByteString)
authPOP3 hSock user pass = do
  sendAllBuilder hSock 1024 (userBuilder "" "\r\n") mempty
  (state,str1) <- readPOPR hSock mempty
  let b1 = userBuilder ">> " "\n"  `mappend` byteString str1
  if state then do
    sendAllBuilder hSock 1024 (passBuilder "" "\r\n") mempty
    (is, str2) <- readPOPR hSock mempty
    let b2 = passBuilder ">> " "\n" `mappend` byteString str2
        rt2 = BL.toStrict $ toLazyByteString $ b1 `mappend` b2
    return (is,rt2)
    else do
    let rt1 = BL.toStrict $ toLazyByteString b1
    return (False,rt1)
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
statPOP3 :: Socket f Stream TCP -> IO (Int,Int,B.ByteString) 
statPOP3 hSock = do
  sendAllBuilder hSock 1024 (statBuilder "" "\r\n") mempty
  (state,str) <- readPOPR hSock mempty
  let b = statBuilder ">> " "\n" `mappend` byteString str
      (t,s) = if state then getPOPSTAT str else (0,0)
      rt = BL.toStrict $ toLazyByteString b
  return (t,s,rt)
  where statBuilder prefix postfix = foldr mappend mempty
          [ byteString prefix
          , byteString "STAT"
          , byteString postfix
          ]
\end{code}

\begin{code}
retrPOP3 :: Socket f Stream TCP -> Int -> Int -> IO (B.ByteString)
retrPOP3 hSock i t  = do
  sendAllBuilder hSock 1024 (retrBuilder "" "\r\n") mempty
  (state,str1) <- readPOPR hSock mempty
  str2 <- if state then readUntilDotLine hSock mempty
          else return ""
  return $ BL.toStrict $ toLazyByteString $ retrBuilder ">> " "\n"
    `mappend` byteString str1
    `mappend` byteString str2
  where retrBuilder prefix postfix = foldr mappend mempty
          [ byteString prefix
          , byteString "RETR "
          , byteString (BC.pack $ show $ mod (i - 1) t + 1)
          , byteString postfix
          ]
\end{code}

\begin{code}
connectPOP3auth :: forall f .HasAddressInfo f
                => B.ByteString
                -> B.ByteString -> B.ByteString
                -> IO (Socket f Stream TCP,Bool,B.ByteString)
connectPOP3auth url user pass = do
  (hSock,str1) <- connectPOP3 url
  (is,str2) <- authPOP3 hSock user pass
  return (hSock,is,str1 `B.append` str2)
\end{code}


\begin{code}
readPOPR :: Socket f Stream TCP -> MessageFlags -> IO (Bool,B.ByteString)
readPOPR hSock msgf = getPOPR <$> readUntilEndLine hSock msgf
\end{code}
