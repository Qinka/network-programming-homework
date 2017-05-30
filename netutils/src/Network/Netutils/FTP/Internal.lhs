
\subsection{FTP Internal Communication}
\label{sec:ftp:internal}
\begin{code}
module Network.Netutils.FTP.Internal
       ( connectFTP
       , closeFTP
       , pasvFTP
       , listFTP
       , authFTP
       , connectFTPauth
       , listFTPpasv
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
           -> IO (Socket f Stream TCP,B.ByteString)
connectFTP ftpUrl = do
  hSock <- socket
  case parsingURL ftpUrl of
    Left err -> error $ BC.unpack err
    Right URL{..} -> do
      let realPort = fromMaybe urlProtocal urlPort
      addr <- head <$> getAddressInfo (Just urlHostName)
        (Just realPort) mempty :: HasAddressInfo f
                               => IO (AddressInfo f Stream TCP)
      connect hSock $ socketAddress addr
      (_,_,rt) <- readFTPR hSock mempty
      return (hSock,rt)
\end{code}

\begin{code}
closeFTP :: Socket f Stream TCP -> IO B.ByteString
closeFTP hSock = do
  sendAllBuilder hSock 1024 (quitBuilder "" "\r\n") mempty
  (_,_,rt) <- readFTPR hSock mempty
  return $ BL.toStrict $ toLazyByteString $ quitBuilder ">> " "\n" `mappend` byteString rt
  where quitBuilder prefix postfix = foldr mappend mempty
          [ byteString prefix
          , byteString "QUIT"
          , byteString postfix
          ]
\end{code}

\begin{code}
authFTP :: Socket f Stream TCP
        -> B.ByteString -> B.ByteString
        -> IO (Bool,B.ByteString)
authFTP hSock user pass = do
  sendAllBuilder hSock 1024 (userBuilder "" "\r\n") mempty
  (id1,_,rt1) <- readFTPR hSock mempty
  if id1 == 331
    then do
    sendAllBuilder hSock 1024 (passBuilder "" "\r\n") mempty
    (id2,_,rt2) <- readFTPR hSock mempty
    return ( id2 == 230,BL.toStrict $ toLazyByteString $ foldr mappend mempty
                        [ userBuilder ">> " "\n"
                        , byteString rt1
                        , passBuilder ">> " "\n"
                        , byteString rt2
                        ]
           )
    else do
    return ( False,BL.toStrict $ toLazyByteString $ foldr mappend mempty
                   [ userBuilder ">> " "\n"
                   , byteString rt1
                   ]
           )
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
pasvFTP :: HasAddressInfo f => Socket f Stream TCP -> IO (Maybe (Socket f Stream TCP),B.ByteString)
pasvFTP hSock = do
  sendAllBuilder hSock 1024 (pasvBuilder "" "\r\n") mempty
  (id,info,rt) <- readFTPR hSock mempty
  pS <- if id == 227 then do
    let (host,port) = getFTPPASV info
    addr <- head <$> getAddressInfo (Just host)
      (Just port) mempty  :: HasAddressInfo f
                          => IO (AddressInfo f Stream TCP)
    pSock <- socket
    connect pSock $ socketAddress addr
    return $ Just pSock
    else return Nothing
  return (pS,BL.toStrict $ toLazyByteString $ foldr mappend mempty
             [ pasvBuilder ">> " "\n"
             , byteString rt
             ]
         )
  where pasvBuilder prefix postfix = foldr mappend mempty
          [ byteString prefix
          , byteString "PASV"
          , byteString postfix
          ]
\end{code}


\begin{code}
listFTP :: Socket f Stream TCP -> Socket f Stream TCP -> IO (B.ByteString,B.ByteString)
listFTP hSock pSock = do
  sendAllBuilder hSock 1024 (listBuilder "" "\r\n") mempty
  (id1,_,str1) <- readFTPR hSock mempty
  if id1 == 150
    then do
    (id2,_,str2) <- readFTPR hSock mempty
    if id2 == 226
      then do
      strd <- receiveAll pSock 10 mempty
      return ( BL.toStrict $ toLazyByteString $ foldr mappend mempty
               [ listBuilder ">> " "\n"
               , byteString str1
               , byteString str2
               ]
             , BL.toStrict $ strd
             )
      else return ( BL.toStrict $ toLazyByteString $ foldr mappend mempty
                    [ listBuilder ">> " "\n"
                    , byteString str1
                    , byteString str2
                    ]
                  , ""
                  )
    else return ( BL.toStrict $ toLazyByteString $ foldr mappend mempty
                  [ listBuilder ">> " "\n"
                  , byteString str1
                  ]
                , ""
                )
  where listBuilder prefix postfix = foldr mappend mempty
                                           [ byteString prefix
          , byteString "LIST"
          , byteString postfix
          ]
\end{code}

\begin{code}
readFTPR :: Socket f Stream TCP -> MessageFlags -> IO (Int,B.ByteString,B.ByteString)
readFTPR hSock msgf = fixM (readUntilEndLine hSock msgf) mempty mempty >>=
                      \(i,b1,b2) -> return (i,BL.toStrict $ toLazyByteString b1,BL.toStrict $ toLazyByteString b2)
  where fixM mf infoB allB = do
          str <- mf
          case getFTPR str of
            (Just (-1),e) -> error $ BC.unpack e
            (Just id,info) -> return (id,infoB `mappend` byteString info, allB `mappend` byteString str)
            (Nothing,info) -> fixM mf ( infoB `mappend` byteString info) (allB `mappend` byteString str)
\end{code}

\begin{code}
connectFTPauth :: forall f.HasAddressInfo f
                   => B.ByteString
                   -> B.ByteString -> B.ByteString
                   -> IO (Socket f Stream TCP,Bool,B.ByteString)
connectFTPauth url user pass = do
  (hSock,str1) <- connectFTP url
  (is,str2) <- authFTP hSock user pass
  return (hSock,is,str1 `B.append` str2)
\end{code}

\begin{code}
listFTPpasv :: HasAddressInfo f => Socket f Stream TCP -> IO (B.ByteString,B.ByteString)
listFTPpasv hSock = do
  (pS',str1) <- pasvFTP hSock
  case pS' of
    Just pSock -> do
      (str2,strd) <- listFTP  hSock pSock
      return (str1 `B.append` str2,strd)
    Nothing -> return (str1,"")
\end{code}
