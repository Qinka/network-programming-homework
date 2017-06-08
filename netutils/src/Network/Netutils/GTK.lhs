
\subsection{{GTK+}}
\label{sec:oc:gtk}



\begin{code}
module Network.Netutils.GTK
       ( initGUI
       ) where

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import qualified Data.GI.Base as Base
import System.Mem(performGC)
\end{code}

This section is about the initial method of the GTK re-encapsulated.

\begin{code}
initGUI :: IO ()
initGUI = do
  GLib.timeoutAdd 0 50000 $ do
    putStrLn "** (T) Going into GC"
    performGC
    putStrLn "** GC done"
    return True
  Gtk.init Nothing
  return ()
\end{code}
