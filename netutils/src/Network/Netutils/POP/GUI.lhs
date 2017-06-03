
\begin{code}
module Network.Neturils.POP.GUI
       (
       ) where

import Network.Netutils.POP.Internal
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
data POPWindow f = POPWindow
                   { popWindows :: (Window,Window) -- panel, verbose
                   , popView :: TextView -- verbose
                   , popButtons :: (Button,Button,Button,Button) -- connect, stat, retr, quit
                   , popEntries :: (Entry,Entry,Entry,Entry) -- url, user, pass, id
                   , popSocket :: IORef (Socket f Stream TCP)
                   , popTotal  :: IORef Int
                   }                   
\end{code}

\begin{code}
mkPOPWindow :: Family f => IO (FTPWindow f)
mkPOPWindow = do
  winPanel   <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 200
                           , #defaultHeight := 150, #title := "Panel"]
  winVerbose <- new Window [ #type := WindowTypeToplevel, #defaultWidth := 200
                           , #defaultHeight := 150, #title := "Verbose"]
  on winVerbose #destory $ #close winPanel   >> mainQuit
  on winPanel   #destory $ #close winVerbose >> mainQuit
  swVerbose <- new ScrolledWindow []
  tvVerbose <- new TextView [#editable := False]
  #add swVerbose tvVerbose
  #add winVerbose swVerbose
  hSock <- socket >>= newIORef
  lUrl  <- new Label [#label := "URL:"]
  eUrl  <- new Entry []
  lUser <- new Label [#label := "User:"]
  eUser <- new Entry []
  lPass <- new Label [#label := "Password:"]
  ePass <- new Entry [#visibility := False] -- , #invisibleChar := '*']
  lMail <- new Label [#label := "Mail id:"]
  eMail <- new Entry []
  bConn <- new Button [#sensitive := False, #label := "Connect"]
  bStat <- new Button [#sensitive := False, #label := "Stat"]
  bRetr <- new Button [#sensitive := False, #label := "Retr"]
  bQuit <- new Button [#sensitive := False, #label := "Quit"]
  grid <- new Grid [];
  grid `#attach` lUrl  0  0   15 5
  grid `#attach` eUrl  15 0   25 5
  grid `#attach` lUser 0  20  15 5
  grid `#attach` eUser 15 20  25 5
  grid `#attach` lPass 0  40  15 5
  grid `#attach` ePass 15 40  25 5
  grid `#attach` lMail 0  60  15 5
  grid `#attach` eMail 15 60  25 5
  grid `#attach` bConn 1  80  38 5
  grid `#attach` bStat 1  100 38 5
  grid `#attach` bRetr 1  120 38 5
  grid `#attach` bQuit 1  140 38 5
  #add winPanel grid
\end{code}
