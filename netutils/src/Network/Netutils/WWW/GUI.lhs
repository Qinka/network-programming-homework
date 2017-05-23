

\subsection{HTTP GUI}
\label{sec:http:gui}
\begin{code}
module Network.Netutils.WWW.GUI
       ( updateHTTPItem
       , initWWW
       , mkMainWindow
       , mkHeadBar
       , mkTextView
       , guiStartX
       ) where


import Network.Netutils.WWW.HTTP
import System.Mem(performGC)

import GI.Gtk hiding(init,main)
import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import Data.GI.Base

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import Data.Text.Encoding
\end{code}


\begin{code}
updateHTTPItem :: Entry -> TextView -> IO ()
updateHTTPItem urlEntry textview = void $ do
  url <- urlEntry `get` #text
  mv  <- newEmptyMVar
  forkIO $ getHttp mv $ encodeUtf8 url
  forkIO $ renew   mv textview
  where getHttp mv url = void $ do
          rep <- doHTTPv4 url
          mv `putMVar` rep
        renew mv tView = void $ do
          req <- takeMVar mv
          buf <- tView `get` #buffer
          buf `set` [#text := decodeUtf8 req]
\end{code}

\begin{code}
initWWW :: IO ()
initWWW = do
  GLib.timeoutAdd 0 5000 $ do
    putStrLn "** (T) Going into GC"
    performGC
    putStrLn "** GC done"
    return True
  Gtk.init Nothing
  return ()
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
  textView <- new TextView [ #editable := False ]
  #add win textView
  return textView
\end{code}

\begin{code}
guiStartX :: Window -> IO ()
guiStartX win = do
  #showAll win
  Gtk.main
\end{code}


