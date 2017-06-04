
\begin{code}
module Network.Netutils.ScanPort.GUI
       ( SPTWindow
       , initGUI
       , mkSPTWindow
       , mkFieldCheckEvent
       , mkButtonsClickedEvent
       , displayAll
       ) where

import Network.Netutils.ScanPort.Internal
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

\begin{code}
data SPTWindow = SPTWindow { sptWindows :: (Window,Window,Window) -- panel, tcp, udp
                           , sptViews :: (TextView,TextView) -- tcp,udp
                           , sptButtons :: (Button,Button)
                           , sptEntries :: [Entry]
                           , sptState :: (IORef Bool,IORef Bool)
                           }
\end{code}

\begin{code}
mkSPTWindow :: IO SPTWindow
mkSPTWindow = do
  winPanel <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 200
                         , #defaultHeight := 150, #title := "Panel"]
  winTCP   <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 200
                         , #defaultHeight := 150, #title := "Result of Scanning TCP Ports"]
  winUDP   <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 200
                         , #defaultHeight := 150, #title := "Result of Scanning UDP Ports"]
  on winPanel #destroy $ #close winTCP   >> #close winUDP >> mainQuit
  on winTCP   #destroy $ #close winPanel >> #close winUDP >> mainQuit
  on winUDP   #destroy $ #close winPanel >> #close winTCP >> mainQuit
  swTCP <- new ScrolledWindow []
  tvTCP <- new TextView [#editable := False]
  #add swTCP  tvTCP
  #add winTCP swTCP
  swUDP <- new ScrolledWindow []
  tvUDP <- new TextView [#editable := False]
  #add swUDP  tvUDP
  #add winUDP swUDP
  tcpS <- newIORef False
  udpS <- newIORef False
  lIP  <- new Label [#label := "IP"]
  eIP1 <- new Entry []
  eIP2 <- new Entry []
  eIP3 <- new Entry []
  eIP4 <- new Entry []
  lPort  <- new Label [#label := "Port"]
  ePortB <- new Entry []
  ePortE <- new Entry []
  bTCP <- new Button [#label := "Start Scan(TCP)", #sensitive := False]
  bUDP <- new Button [#label := "Start Scan(UDP)", #sensitive := False]
  grid <- new Grid []
  #attach grid lIP    0  0   25 5
  #attach grid eIP1   1  20  5  5
  #attach grid eIP2   7  20  5  5
  #attach grid eIP3   13 20  5  5
  #attach grid eIP4   19 20  5  5
  #attach grid lPort  0  40  25 5
  #attach grid ePortB 1  60  10 5
  #attach grid ePortE 14 60  10 5
  #attach grid bTCP   1  80  10 5
  #attach grid bUDP   14 80  10 5
  #add winPanel grid
  return $ SPTWindow { sptWindows = (winPanel,winTCP,winUDP)
                     , sptViews   = (tvTCP,tvUDP)
                     , sptButtons = (bTCP,bUDP)
                     , sptEntries = [eIP1,eIP2,eIP3,eIP4,ePortB,ePortE]
                     , sptState   = (tcpS,udpS)
                     }
\end{code}

\begin{code}
mkFieldCheckEvent :: SPTWindow -> IO ()
mkFieldCheckEvent SPTWindow{..} = void $ do
  let isEmptyText e =
        e `get` #buffer >>= \buf -> T.null <$> buf `get` #text
      checkFields = do
        rts <- not.or <$> mapM isEmptyText sptEntries
        fst sptButtons `set` [#sensitive := rts]
        snd sptButtons `set` [#sensitive := rts]
  mapM_ (\e -> on e #activate checkFields) sptEntries
\end{code}

\begin{code}
mkButtonsClickedEvent :: SPTWindow -> IO ()
mkButtonsClickedEvent SPTWindow{..} = void $ do
  let sTCP addr = fst <$> (scanPortTCP 100000 addr)
      sUDP addr = fst <$> (scanPortUDP 100000 addr)
      (bTCP,bUDP) = sptButtons
      (tvTCP,tvUDP) = sptViews
      (sigTCP,sigUDP) = sptState
  on bTCP #clicked $ action sigTCP sTCP (tvTCP,bTCP) ("Start Scan(TCP)","Stop Scan(TCP)")
  on bUDP #clicked $ action sigUDP sUDP (tvUDP,bUDP) ("Start Scan(UDP)","Stop Scan(UDP)")
  where action sig scan its lab = do
          st <- readIORef sig
          if st then do
            writeIORef sig False
            esS <- checkFields
            snd its `set` [#label := fst lab, #sensitive := esS]
            else void $ do
            buf <- fst its `get` #buffer
            buf `set` [#text := ""]
            writeIORef sig True
            snd its `set` [#label := snd lab]
            let getNumber e = do
                  b <- e `get` #buffer
                  t <- b `get` #text
                  return $ read $ T.unpack t
            e1:e2:e3:e4:eB:eE:_ <- mapM getNumber sptEntries
            let addrs = [ (fromIntegral e1,fromIntegral e2,fromIntegral e3,fromIntegral e4,r)
                        | r <- mkPortRange eB eE]
            forkIO $ void $
              loopScan addrs scan sig 0 its lab
        isEmptyText e =
          e `get` #buffer >>= \buf -> T.null <$> buf `get` #text
        checkFields = not . or <$> mapM isEmptyText sptEntries
        loopScan [] _ sig _ its lab = do
          print 1
          checkFields >>= \isState -> snd its `set` [#label := fst lab, #sensitive := isState]
          print 2
          writeIORef sig False
          print 3
          return False
        loopScan (i@(a,b,c,d,e):is) scan sig count its lab = do
          when (count `mod` 10 == 9) yield
          sigS <- readIORef sig
          if sigS then do
            rt <- scan $ mkInetAddr (a,b,c,d) e
            threadDelay 100000
            buf <- fst its `get` #buffer
            end <- textBufferGetEndIter buf :: IO TextIter
            dis (rt,i) buf end
            loopScan is scan sig (mod (count+1) 10) its lab
            else do
            checkFields >>= \isState -> snd its `set` [#label := fst lab, #sensitive := isState]
            writeIORef sig True
            return False
        dis :: (Bool,(Word8,Word8,Word8,Word8,Word16)) -> TextBuffer -> TextIter -> IO ()
        dis (is,ip) buf end = void $
          #insert buf end (disIP is ip) (-1)
        disIP is (a,b,c,d,e) =
          fmt     $ padLeftF 3 ' ' a
          `mappend` "."
          `mappend` padLeftF 3 ' ' b
          `mappend` "."
          `mappend` padLeftF 3 ' ' c
          `mappend` "."
          `mappend` padLeftF 3 ' ' d
          `mappend` ":"
          `mappend` padLeftF 5 ' ' e
          `mappend` padLeftF 8 ' ' (if is then "Open\n" else "Close\n" :: String)          
        mkPortRange a' b' =
          let a = max a' 0
              b = min b' 65535
          in [a .. b]
\end{code}

              
\begin{code}
displayAll :: SPTWindow -> IO ()
displayAll SPTWindow{..} = do
  let (winPanel,winTCP,winUDP) = sptWindows
  #showAll winPanel
  #showAll winTCP
  #showAll winUDP
  Gtk.main
\end{code}
