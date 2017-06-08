
\section{POP3 Protocol}
\label{sec:pop}

\begin{code}
module Network.Netutils.POP
       ( main
       ) where

import Network.Netutils.Socket
import Network.Netutils.POP.GUI
\end{code}

In this section, there are the codes about the a mail protocol -- POP3.
There the first part of the codes is about the main method of the this program.
The next part is the codes about the communication with pop3 server.
The final part is about the GUI.

\begin{code}
main :: IO ()
main = do
  initGUI
  win <- mkPOPWindow :: IO (POPWindow Inet)
  mkFieldCheckEvent     win
  mkButtonsClickedEvent win
  displayAll            win
\end{code}

\inputHaskell{src/Network/Netutils/POP/Internal.lhs}
\inputHaskell{src/Network/Netutils/POP/GUI.lhs}

\subsection{Application}
\label{sec:pop:app}

The figure \ref{fig:pop} is the snap shot of the sceen.

\begin{figure}[h]
  \centering
  \includegraphics[width=1\linewidth]{img/pop}
  \caption{The sceen shot of the application}
  \label{fig:pop}
\end{figure}