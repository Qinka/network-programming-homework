\begin{code}
module Network.Netutils.POP
       ( main
       ) where

import Network.Netutils.Socket
import Network.Netutils.POP.GUI
\end{code}

\begin{code}
main :: IO ()
main = do
  initGUI
  win <- mkPOPWindow :: IO (POPWindow Inet)
  mkFieldCheckEvent     win
  mkButtonsClickedEvent win
  displayAll            win
\end{code}
