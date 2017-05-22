

\subsection{HTTP GUI}
\label{sec:http:gui}
\begin{code}
module Network.Netutils.WWW.GUI
       (
       ) where

import qualified GI.Gtk as Gtk
import Data.GI.Base

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
\end{code}


\begin{code}
updateHTTPItem :: Entry -> TextView -> IO ()
updateHTTPItem urlEntry textview = void $ do
  url <- urlEntry `get` #text
  mv  <- newEmptyMVar
  forkIO $ getHttp mv
  forkIO $ renew   mv
\end{code}
