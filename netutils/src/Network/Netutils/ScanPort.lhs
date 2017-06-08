
\section{Scan Port}
\label{sec:scanport}

\begin{code}
module Network.Netutils.ScanPort
       ( main
       ) where

import Network.Netutils.ScanPort.GUI
\end{code}

In this section, the codes is about scanning the ports of a remote host.
The first part of the codes is about the main method of the program.
Next part of the codes is about how  program scan the TCP \& UDP ports.
Finally, the last part of the codes is about the GUI.

\begin{code}
main :: IO ()
main = do
  initGUI
  win <- mkSPTWindow
  mkFieldCheckEvent win
  mkButtonsClickedEvent win
  displayAll win
\end{code}

\inputHaskell{src/Network/Netutils/ScanPort/Internal.lhs}
\inputHaskell{src/Network/Netutils/ScanPort/GUI.lhs}

\subsection{Application}
\label{sec:scanport:app}

The figure \ref{fig:scanport} is the snap shot of the sceen.

\begin{figure}[h]
  \centering
  \includegraphics[width=1\linewidth]{img/scanport}
  \caption{The sceen shot of the application}
  \label{fig:scanport}
\end{figure}
