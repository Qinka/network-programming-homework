\begin{code}
module Network.Netutils.ScanIP
       ( main
       ) where

import Network.Netutils.Socket
import Network.Netutils.ScanIP.GUI
\end{code}

\begin{code}
main :: IO ()
main = do
  initGUI
  win <- mkSIPWindow
  mkFieldCheckEvent win
  mkButtonClickedEvent win
  displayAll win
\end{code}

