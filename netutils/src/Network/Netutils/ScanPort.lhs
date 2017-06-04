\begin{code}
module Network.Netutils.ScanPort
       ( main
       ) where

import Network.Netutils.ScanPort.GUI
\end{code}

\begin{code}
main :: IO ()
main = do
  initGUI
  win <- mkSPTWindow
  mkFieldCheckEvent win
  mkButtonsClickedEvent win
  displayAll win
\end{code}
