\section{HTTP}
\label{sec:http}
\begin{code}
module Network.Netutils.WWW
       ( wwwMain
       ) where
import Network.Netutils.WWW.GUI
import GI.Gtk hiding(init,main)
import qualified GI.Gtk as Gtk
\end{code}

\begin{code}
wwwMain :: IO ()
wwwMain = do
  initWWW
  win <- mkMainWindow
  textview <- mkTextView win
  (header,urlEntry) <- mkHeadBar win
  on urlEntry #activate $ updateHTTPItem urlEntry textview
  guiStartX win
\end{code}
