

\begin{code}
module Network.Netutils.FTP.GUI
       ( FTPWindow
       , initFTP
       , mkFTPWindow
       , mkFieldCheckEvent
       , mkButtonsClickedEvent
       , displayAll
       , initGUI
       ) where


import Network.Netutils.FTP.Internal
import Network.Netutils.Socket
import Network.Netutils.GTK hiding(init,main)
import qualified Network.Netutils.GTK as Gtk

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.Text.Encoding
import Data.IORef

import qualified Data.Text       as T
import qualified Data.ByteString as B
\end{code}

\begin{code}
data FTPWindow f = FTPWindow
                   { ftpWindows :: (Window,Window,Window) -- panel, verbose, data
                   , ftpViews  :: (TextView,TextView) -- verbose, data
                   , ftpButtons :: (Button,Button,Button) -- connect, list, quit
                   , ftpEntries  :: (Entry,Entry,Entry) -- url, user, pass
                   , ftpSocket  :: IORef (Socket f Stream TCP)
                   }
\end{code}



\begin{code}
mkFTPWindow :: Family f => IO (FTPWindow f)
mkFTPWindow = do
  winPanel   <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 200
                           , #defaultHeight := 150, #title := "Panel"]
  winVerbose <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 200
                           , #defaultHeight := 150, #title := "Verbose"]
  winData    <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 100
                           , #defaultHeight := 300, #title := "Data"]
  on winPanel   #destroy $ #close winVerbose >> #close winData    >> mainQuit
  on winVerbose #destroy $ #close winPanel   >> #close winData    >> mainQuit
  on winData    #destroy $ #close winPanel   >> #close winVerbose >> mainQuit
  swVerbose <- new ScrolledWindow []
  tvVerbose <- new TextView [#editable := False]
  #add swVerbose  tvVerbose
  #add winVerbose swVerbose
  swData <- new ScrolledWindow []
  tvData <- new TextView [#editable := False]
  #add swData  tvData
  #add winData swData
  hSock <- socket >>= newIORef
  lUrl  <- new Label [#label := "URL:"]
  eUrl  <- new Entry []
  lUser <- new Label [#label := "User:"]
  eUser <- new Entry []
  lPass <- new Label [#label := "Password:"]
  ePass <- new Entry [#visibility := False] -- , #invisibleChar := '*']
  bConn <- new Button [#sensitive := False, #label := "Connect"]
  bList <- new Button [#sensitive := False, #label := "List"]
  bQuit <- new Button [#sensitive := False, #label := "Quit"]
  grid <- new Grid [];
  #attach grid lUrl  0  0   15 5
  #attach grid eUrl  15 0   25 5
  #attach grid lUser 0  20  15 5
  #attach grid eUser 15 20  25 5
  #attach grid lPass 0  40  15 5
  #attach grid ePass 15 40  25 5
  #attach grid bConn 1  60  38 5
  #attach grid bList 1  80  38 5
  #attach grid bQuit 1  100 38 5
  #add winPanel grid
  return $ FTPWindow { ftpWindows = (winPanel,winVerbose,winData)
                     , ftpViews   = (tvVerbose,tvData)
                     , ftpButtons = (bConn,bList,bQuit)
                     , ftpEntries = (eUrl,eUser,ePass)
                     , ftpSocket  = hSock
                     }
\end{code}


\begin{code}
mkFieldCheckEvent :: FTPWindow f -> IO ()
mkFieldCheckEvent v@FTPWindow{..} = do
  let (eUrl,eUser,ePass)  = ftpEntries
      (bConn,bList,bQuit) = ftpButtons
  let checkFields = do
        bufUrl  <- eUrl  `get` #buffer
        bufUser <- eUser `get` #buffer
        bufPass <- ePass `get` #buffer
        iI <- T.null <$> bufUrl  `get` #text
        iU <- T.null <$> bufUser `get` #text
        iP <- T.null <$> bufPass `get` #text
        iL <- bList `get` #sensitive
        iQ <- bQuit `get` #sensitive
        bConn `set` [#sensitive := not (or [iI,iU,iP,iL,iQ])]
  on eUrl  #activate checkFields
  on eUser #activate checkFields
  on ePass #activate checkFields
  return ()
\end{code}

\begin{code}
mkButtonsClickedEvent :: HasAddressInfo f => FTPWindow f -> IO ()
mkButtonsClickedEvent v@FTPWindow{..} = do
  let (eUrl,eUser,ePass)  = ftpEntries
      (bConn,bList,bQuit) = ftpButtons
      (tvVerbose,tvData)  = ftpViews  
  on bConn #clicked $ void $ do
    let getText e = do
          b <- e `get` #buffer
          b `get` #text
    url  <- encodeUtf8 <$> getText eUrl
    user <- encodeUtf8 <$> getText eUser
    pass <- encodeUtf8 <$> getText ePass
    mv <- newEmptyMVar
    threadsAddIdle PRIORITY_DEFAULT $ do
      connectFTPauth url user pass >>= putMVar mv
      return False
    threadsAddIdle PRIORITY_DEFAULT $ do
      (hSock,is,str) <- takeMVar mv
      buff <- tvVerbose `get` #buffer
      end  <- textBufferGetEndIter buff ::  IO TextIter
      #insert buff end (decodeUtf8 str) (-1)
      when is $ void $ do
        writeIORef ftpSocket hSock
        bConn `set` [#sensitive := False]
        bList `set` [#sensitive := True]
        bQuit `set` [#sensitive := True]
      return False
  on bList #clicked $ void $ do
    mv <- newEmptyMVar
    threadsAddIdle PRIORITY_DEFAULT $ do
      hSock <- readIORef ftpSocket
      listFTPpasv hSock >>= putMVar mv
      return False
    threadsAddIdle PRIORITY_DEFAULT $ do
      (str,strd) <- takeMVar mv
      buffV <- tvVerbose `get` #buffer
      end  <- textBufferGetEndIter buffV ::  IO TextIter
      #insert buffV end (decodeUtf8 str) (-1)
      buffD <- tvData `get` #buffer
      buffD `set` [#text := decodeUtf8 strd]
      return False
  on bQuit #clicked $ void $ do
    mv <- newEmptyMVar
    threadsAddIdle PRIORITY_DEFAULT $ do
      hSock <- readIORef ftpSocket
      closeFTP hSock >>= putMVar mv
      return False
    threadsAddIdle PRIORITY_DEFAULT $ do
      str  <- takeMVar mv
      buff <- tvVerbose `get` #buffer
      end  <- textBufferGetEndIter buff ::  IO TextIter
      #insert buff end (decodeUtf8 str) (-1)
      bList `set` [#sensitive := False]
      bQuit `set` [#sensitive := False]
      bufUrl  <- eUrl  `get` #buffer
      bufUser <- eUser `get` #buffer
      bufPass <- ePass `get` #buffer
      iI <- T.null <$> bufUrl  `get` #text
      iU <- T.null <$> bufUser `get` #text
      iP <- T.null <$> bufPass `get` #text
      bConn `set` [#sensitive := not (or [iI,iU,iP])]
      return False
  return ()
\end{code}

  
\begin{code}
displayAll :: FTPWindow f -> IO ()
displayAll FTPWindow{..} = do
  let (winPanel,winVerbose,winData) = ftpWindows
  #showAll winPanel
  #showAll winVerbose
  #showAll winData
  Gtk.main
\end{code}
