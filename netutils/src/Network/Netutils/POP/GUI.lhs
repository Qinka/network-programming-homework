
\subsection{GUI of POP3}
\label{sec:pop:gui}
\begin{code}
module Network.Netutils.POP.GUI
       ( POPWindow
       , initGUI
       , mkPOPWindow
       , mkFieldCheckEvent
       , mkButtonsClickedEvent
       , displayAll
       ) where

import Network.Netutils.POP.Internal
import Network.Netutils.Socket

import Network.Netutils.GTK
import qualified GI.Gdk as Gdk
import GI.Gtk hiding (init,main)
import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import Data.GI.Base

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.Text.Encoding
import Data.IORef

import qualified Data.Text       as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
\end{code}

First of all, the ADT of the window are defined with the following codes.
In this data type, it holds the two windows, one for panel and one for verbose,
a text view to display the details of the communication, a socket for connection,
and others widgets or variables which will bu used.
\begin{code}
data POPWindow f = POPWindow
                   { popWindows :: (Window,Window) -- panel, verbose
                   , popView :: TextView -- verbose
                   , popButtons :: (Button,Button,Button,Button) -- connect, stat, retr, quit
                   , popEntries :: (Entry,Entry,Entry,Entry) -- url, user, pass, id
                   , popSocket :: IORef (Socket f Stream TCP)
                   , popTotal  :: IORef Int
                   }                   
\end{code}

Next one is the method of the windows' constructor.
\begin{code}
mkPOPWindow :: Family f => IO (POPWindow f)
mkPOPWindow = do
  winPanel   <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 200
                           , #defaultHeight := 150, #title := "Panel"]
  winVerbose <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 200
                           , #defaultHeight := 150, #title := "Verbose"]
  on winVerbose #destroy $ #close winPanel   >> mainQuit
  on winPanel   #destroy $ #close winVerbose >> mainQuit
  swVerbose <- new ScrolledWindow []
  tvVerbose <- new TextView [#editable := False]
  #add swVerbose tvVerbose
  #add winVerbose swVerbose
  hSock <- socket >>= newIORef
  tot <- newIORef 0
  lUrl  <- new Label [#label := "URL:"]
  eUrl  <- new Entry []
  lUser <- new Label [#label := "User:"]
  eUser <- new Entry []
  lPass <- new Label [#label := "Password:"]
  ePass <- new Entry [#visibility := False]
  lMail <- new Label [#label := "Mail id:"]
  eMail <- new Entry []
  bConn <- new Button [#sensitive := False, #label := "Connect"]
  bStat <- new Button [#sensitive := False, #label := "Stat"]
  bRetr <- new Button [#sensitive := False, #label := "Retr"]
  bQuit <- new Button [#sensitive := False, #label := "Quit"]
  grid <- new Grid []
  #attach grid lUrl  0  0   15 5
  #attach grid eUrl  15 0   25 5
  #attach grid lUser 0  20  15 5
  #attach grid eUser 15 20  25 5
  #attach grid lPass 0  40  15 5
  #attach grid ePass 15 40  25 5
  #attach grid lMail 0  60  15 5
  #attach grid eMail 15 60  25 5
  #attach grid bConn 1  80  38 5
  #attach grid bStat 1  100 38 5
  #attach grid bRetr 1  120 38 5
  #attach grid bQuit 1  140 38 5
  #add winPanel grid
  return $ POPWindow { popWindows = (winPanel, winVerbose)
                     , popView    = tvVerbose
                     , popButtons = (bConn,bStat,bRetr,bQuit)
                     , popEntries = (eUrl,eUser,ePass,eMail)
                     , popSocket  = hSock
                     , popTotal   = tot
                     }
\end{code}
The method to add the checking event to entries.
\begin{code}
mkFieldCheckEvent :: POPWindow f -> IO ()
mkFieldCheckEvent v@POPWindow{..} = void $ do
  let (eUrl,eUser,ePass,eMail)  = popEntries
      (bConn,bStat,bRetr,bQuit) = popButtons
  let checkFields = do
        bufUrl  <- eUrl  `get` #buffer
        bufUser <- eUser `get` #buffer
        bufPass <- ePass `get` #buffer
        iI <- T.null <$> bufUrl  `get` #text
        iU <- T.null <$> bufUser `get` #text
        iP <- T.null <$> bufPass `get` #text
        iL <- bStat `get` #sensitive
        iQ <- bQuit `get` #sensitive
        bConn `set` [#sensitive := not (or [iI,iU,iP,iL,iQ])]
  on eUrl  #activate checkFields
  on eUser #activate checkFields
  on ePass #activate checkFields
  on eMail #activate $ void $ do
    total <- readIORef popTotal
    bufMail <- eMail `get` #buffer
    iM <- T.null <$> bufMail `get` #text
    let iT = total <= 0
    bRetr `set` [#sensitive := not ( iT || iM)]
\end{code}
The method to add the clicked event handler of the button.
\begin{code}
mkButtonsClickedEvent :: HasAddressInfo f => POPWindow f -> IO ()
mkButtonsClickedEvent v@POPWindow{..} = void $ do
  let (eUrl,eUser,ePass,eMail)  = popEntries
      (bConn,bStat,bRetr,bQuit) = popButtons
      tvVerbose                 = popView
  on bConn #clicked $ void $ do
    let getText e = e `get` #buffer >>= (`get` #text)
    url  <- encodeUtf8 <$> getText eUrl
    user <- encodeUtf8 <$> getText eUser
    pass <- encodeUtf8 <$> getText ePass
    mv   <- newEmptyMVar
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      connectPOP3auth url user pass >>= putMVar mv
      return False
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      (hSock, is, str) <- takeMVar mv
      buff <- tvVerbose `get` #buffer
      end <- textBufferGetEndIter buff :: IO TextIter
      #insert buff end (decodeUtf8 str) (-1)
      when is $ void $ do
        writeIORef popSocket hSock
        bConn `set` [#sensitive := False]
        bStat `set` [#sensitive := True ]
        bQuit `set` [#sensitive := True ]
      return False
  on bStat #clicked $ void $ do
    mv <- newEmptyMVar
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      hSock <- readIORef popSocket
      statPOP3 hSock >>= putMVar mv
      return False
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      (tot,_,str) <- takeMVar mv
      writeIORef popTotal tot
      buffV <- tvVerbose `get` #buffer
      end <- textBufferGetEndIter buffV :: IO TextIter
      #insert buffV end (decodeUtf8 str) (-1)
      return False
  on bRetr #clicked $ void $ do
    mv <- newEmptyMVar
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      hSock <- readIORef popSocket
      tot <- readIORef popTotal
      buf <- eMail `get` #buffer
      no <- T.unpack <$> buf `get` #text
      retrPOP3 hSock (read no) tot >>= putMVar mv
      return False
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      str <- takeMVar mv
      buffV <- tvVerbose `get` #buffer
      end <- textBufferGetEndIter buffV :: IO TextIter
      #insert buffV end (T.pack $ BC.unpack str) (-1)
      return False
  on bQuit #clicked $ void $ do
    mv <- newEmptyMVar
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      hSock <- readIORef popSocket
      quitPOP3 hSock >>= putMVar mv
      return False
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
      str  <- takeMVar mv
      buff <- tvVerbose `get` #buffer
      end  <- textBufferGetEndIter buff ::  IO TextIter
      #insert buff end (decodeUtf8 str) (-1)
      bStat `set` [#sensitive := False]
      bRetr `set` [#sensitive := False]
      bQuit `set` [#sensitive := False]
      bufUrl  <- eUrl  `get` #buffer
      bufUser <- eUser `get` #buffer
      bufPass <- ePass `get` #buffer
      iI <- T.null <$> bufUrl  `get` #text
      iU <- T.null <$> bufUser `get` #text
      iP <- T.null <$> bufPass `get` #text
      bConn `set` [#sensitive := not (or [iI,iU,iP])]
      return False      
\end{code}
Finaly, the method to display all the widgets and windows.
\begin{code}
displayAll :: POPWindow f -> IO ()
displayAll POPWindow{..} = do
  let (winPanel,winVerbose) = popWindows
  #showAll winPanel
  #showAll winVerbose
  Gtk.main
\end{code}
