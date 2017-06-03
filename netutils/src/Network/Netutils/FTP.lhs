

\begin{code}
module Network.Netutils.FTP
       ( main
       ) where

import Network.Netutils.Socket
import Network.Netutils.FTP.GUI
\end{code}

\begin{code}
main :: IO ()
main = do
  initGUI
  win <- mkFTPWindow :: IO (FTPWindow Inet)
  mkFieldCheckEvent win
  mkButtonsClickedEvent win
  displayAll win
\end{code}
