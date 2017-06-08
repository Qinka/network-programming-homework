
\section{HTTP}
\label{sec:http}
\begin{code}
module Network.Netutils.WWW
       ( main
       ) where
import Network.Netutils.WWW.GUI
import GI.Gtk hiding(init,main)
import qualified GI.Gtk as Gtk
\end{code}

In this section, there are the codes about the the \verb|GET| requestion in HTTP.
The first part of the codes is the main method of this program, and the real executable one
will be in at section \ref{sec:executable}. 
The second part is about how to send the HTTP request with the raw TCP Socket.
Then the final part is about the GUI with GTK+.

\begin{code}
main :: IO ()
main = do
  initGUI
  win <- mkMainWindow
  textview <- mkTextView win
  (header,urlEntry) <- mkHeadBar win
  on urlEntry #activate $ updateHTTPItem urlEntry textview
  guiStartX win
\end{code}

\inputHaskell{src/Network/Netutils/WWW/HTTP.lhs}
\inputHaskell{src/Network/Netutils/WWW/GUI.lhs}

\subsection{Application}
\label{sec:http:app}

The figure \ref{fig:www} is the snap shot of the sceen.

\begin{figure}[h]
  \centering
  \includegraphics[width=1\linewidth]{img/www}
  \caption{The sceen shot of the application}
  \label{fig:www}
\end{figure}

