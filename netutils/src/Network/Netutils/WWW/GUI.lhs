

\subsection{HTTP GUI}
\label{sec:http:gui}
\begin{code}
module Network.Netutils.WWW.GUI
       ( updateHTTPItem
       , initGUI
       , mkMainWindow
       , mkHeadBar
       , mkTextView
       , guiStartX
       ) where


import Network.Netutils.WWW.HTTP

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

import qualified Data.Text       as T
import qualified Data.ByteString as B
\end{code}


\begin{code}
updateHTTPItem :: Entry -> TextView -> IO ()
updateHTTPItem urlEntry textview = void $ do
  url <- urlEntry `get` #text
  when (not $ T.null url) $ void $ do
    mv  <- newEmptyMVar
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ getHttp mv $ encodeUtf8 url
    Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ renew   mv textview
  where getHttp mv url = do
          rep <- doHTTPv4 url
          print rep
          mv `putMVar` rep
          return False
        renew mv tView = do
          req' <- takeMVar mv
          let req = decodeUtf8 req'
          buf <- tView `get` #buffer
          buf `set` [#text := req]    
          return False
\end{code}

\begin{code}
mkMainWindow :: IO Window
mkMainWindow = do
  win <- new Window [ #type := WindowTypeToplevel
                    , #defaultWidth := 1024
                    , #defaultHeight := 768
                    ]
  on win #destroy mainQuit
  return win
\end{code}

\begin{code}
mkHeadBar :: Window -> IO (HeaderBar,Entry)
mkHeadBar win = do
  urlEntry <- new Entry [ #placeholderText := "Type the address to load here"
                        , #widthChars := 50
                        ]
  header <- new HeaderBar [ #showCloseButton := True
                        , #customTitle := urlEntry
                        , #title := "HTTP Get METHOD Test"
                        ]
  #setTitlebar win $ Just header
  return (header,urlEntry)
\end{code}

\begin{code}
mkTextView :: Window -> IO (TextView)
mkTextView win = do
  textView <- new TextView [ #editable := False]
  textBuf  <- new TextBuffer []
  textView `set` [#buffer := textBuf]
  sw <- new ScrolledWindow []
  #add sw textView
  #add win sw
  return textView
\end{code}

\begin{code}
guiStartX :: Window -> IO ()
guiStartX win = do
  #showAll win
  Gtk.main
\end{code}


