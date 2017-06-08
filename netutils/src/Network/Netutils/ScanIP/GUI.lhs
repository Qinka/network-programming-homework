
\subsection{GUI}
\label{sec:scanip:gui}

\begin{code}
module Network.Netutils.ScanIP.GUI
       ( SIPWindow
       , initGUI
       , mkSIPWindow
       , mkFieldCheckEvent
       , mkButtonClickedEvent
       , displayAll
       ) where

import Network.Netutils.ScanIP.Internal
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
import Data.Word

import Fmt (fmt,padLeftF)
import qualified Data.Text       as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
\end{code}

Firstly, a ADT for window is needed, where holds two windows, one for panel, and one for output(result),
a button to start or to stop the scanning, a serial entries about the ip address, a text view in the window to display the result,
and others widgets.
\begin{code}
data SIPWindow = SIPWindow
                 { sipWindows :: (Window,Window) -- panel, data
                 , sipButton  :: Button
                 , sipEntries :: [Entry]
                 , sipTextView :: TextView
                 , sipState :: IORef Bool
                 }
\end{code}
The method for windows' constructor.
\begin{code}
mkSIPWindow :: IO SIPWindow
mkSIPWindow = do
  winPanel <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 200
                         , #defaultHeight := 150, #title := "Panel"]
  winData  <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 200
                         , #defaultHeight := 150, #title := "Result"]
  on winPanel #destroy $ #close winData  >> mainQuit
  on winData  #destroy $ #close winPanel >> mainQuit
  swData <- new ScrolledWindow []
  tvData <- new TextView [#editable := False]
  #add swData tvData
  #add winData swData
  ct <- newIORef False
  lBegin <- new Label [#label := "Begin:"]
  lEnd   <- new Label [#label := "End:"  ]
  eIB1   <- new Entry []
  eIB2   <- new Entry []
  eIB3   <- new Entry []
  eIB4   <- new Entry []
  eIE1   <- new Entry []
  eIE2   <- new Entry []
  eIE3   <- new Entry []
  eIE4   <- new Entry []
  lTimeout <- new Label [#label := "Timeout Limit:"]
  eTimeout <- new Entry []
  lTOmicro <- new Label [#label := "microseconds"]
  bOpt <- new Button [#label := "Start Scan",#sensitive := False]
  grid <- new Grid []
  #attach grid lBegin 0  0  25 5
  #attach grid eIB1   1  20 5  5
  #attach grid eIB2   7  20 5  5
  #attach grid eIB3   13 20 5  5
  #attach grid eIB4   19 20 5  5
  #attach grid lEnd   0  40 25 5
  #attach grid eIE1   1  60 5  5
  #attach grid eIE2   7  60 5  5
  #attach grid eIE3   13 60 5  5
  #attach grid eIE4   19 60 5  5
  #attach grid lTimeout 0  80  25 5
  #attach grid eTimeout 0  100 15 5
  #attach grid lTOmicro 16 100 10 5
  #attach grid bOpt     0  120 25 5
  #add winPanel grid
  return $ SIPWindow { sipWindows  = (winPanel,winData)
                     , sipButton   = bOpt
                     , sipEntries  = [eIB1,eIB2,eIB3,eIB4,eIE1,eIE2,eIE3,eIE4,eTimeout]
                     , sipTextView = tvData
                     , sipState    = ct
                     }
\end{code}
The method add the handler when some fields changed.
\begin{code}
mkFieldCheckEvent :: SIPWindow -> IO ()
mkFieldCheckEvent SIPWindow{..} = void $ do
  let isEmptyText tv =
        tv `get` #buffer >>= \buf -> T.null <$> buf `get` #text
      checkFields = do
        rts <- mapM isEmptyText sipEntries
        sipButton `set` [#sensitive := not (or rts)]
  mapM_ (\e -> on e #activate checkFields) sipEntries
\end{code}
The method add the handler when some buttons clicked.
\begin{code}
mkButtonClickedEvent :: SIPWindow -> IO ()
mkButtonClickedEvent SIPWindow{..} = void $ on sipButton #clicked $ void $ do
  st <- readIORef sipState
  if st then do
    writeIORef sipState False
    let isEmptyText tv =
          tv `get` #buffer >>= \buf -> T.null <$> buf `get` #text
        checkFields = not.or <$> mapM isEmptyText sipEntries
    isBS <- checkFields
    sipButton `set` [#label := "Start Scan", #sensitive := isBS]
    else void $ do
    buf' <- sipTextView `get` #buffer
    buf' `set` [#text := ""]
    writeIORef sipState True
    sipButton `set` [#label := "Stop Scan"]
    mv  <- newEmptyMVar
    let getNumber e = do
          b <- e `get` #buffer
          t <- b `get` #text
          return $ read $ T.unpack t
    rts <- mapM getNumber sipEntries
    let ips = [(fromIntegral a,fromIntegral b,fromIntegral c,fromIntegral d)
              | a <- mkRange (rts !! 0, rts !! 4)
              , b <- mkRange (rts !! 1, rts !! 5)
              , c <- mkRange (rts !! 2, rts !! 6)
              , d <- mkRange (rts !! 3, rts !! 7)
              ]
    forkIO $ void $ do
      loopScan ips (rts !! 8) mv sipState 0
  where loopScan [] _ _ _ _ = do
          let isEmptyText tv =
                tv `get` #buffer >>= \buf -> T.null <$> buf `get` #text
              checkFields = not.or <$> mapM isEmptyText sipEntries
          isB <- checkFields
          sipButton `set` [#label := "Start Scan", #sensitive := isB]
          return False
        loopScan (i:is) to mv sig count = do
          when (count `mod` 10 == 9) yield
          ist <- readIORef sig
          if ist then do
            rt <- scanIP to i
            buf <- sipTextView `get` #buffer
            end <- textBufferGetEndIter buf :: IO TextIter
            dis (rt,i) buf end 
            loopScan is to mv sig (count `mod` 10 +1)
            else do
            let isEmptyText tv =
                  tv `get` #buffer >>= \buf -> T.null <$> buf `get` #text
                checkFields = not.or <$> mapM isEmptyText sipEntries
            isB <- checkFields
            sipButton `set` [#label := "Start Scan", #sensitive := isB]
            writeIORef sig True
            return False
        dis :: (Maybe [Word8],(Word8,Word8,Word8,Word8)) -> TextBuffer -> TextIter -> IO ()
        dis (it,ip) buf end = void $ do
          let is = it /= Nothing
          #insert buf end (disIP is ip) (-1)
        disIP :: Bool -> (Word8,Word8,Word8,Word8) -> T.Text
        disIP is (a,b,c,d) =
          fmt     $ padLeftF 3 ' ' a
          `mappend` "."
          `mappend` padLeftF 3 ' ' b
          `mappend` "."
          `mappend` padLeftF 3 ' ' c
          `mappend` "."
          `mappend` padLeftF 3 ' ' d
          `mappend` padLeftF 8 ' ' (if is then "Open\n" else "Close\n" :: String)
        mkRange (a',b') =
          let a = max a' 1
              b = min b' 254
          in [a .. b]
\end{code}
The method to display all the widgets and windows.
\begin{code}
displayAll :: SIPWindow -> IO ()
displayAll SIPWindow{..} = do
  let (winPanel,winData) = sipWindows
  #showAll winPanel
  #showAll winData
  Gtk.main
\end{code}

