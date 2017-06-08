
\section{Scan IP}
\label{sec:scanip}

\begin{code}
module Network.Netutils.ScanIP
       ( main
       ) where

import Network.Netutils.Socket
import Network.Netutils.ScanIP.GUI
\end{code}

In this section, the codes is about scanning IPs.
The first part of the codes is about the main method of the this program.
Next part of the codes is about how the program send the ICMP package and check whether the remote host is online.
The final part is about the GUI.

\begin{code}
main :: IO ()
main = do
  initGUI
  win <- mkSIPWindow
  mkFieldCheckEvent win
  mkButtonClickedEvent win
  displayAll win
\end{code}

\inputHaskell{src/Network/Netutils/ScanIP/Internal.lhs}
\inputHaskell{src/Network/Netutils/ScanIP/GUI.lhs}


\subsection{Application}
\label{sec:scanip:app}

The figure \ref{fig:scanip} is the snap shot of the sceen.

\begin{figure}[h]
  \centering
  \includegraphics[width=1\linewidth]{img/scanip}
  \caption{The sceen shot of the application}
  \label{fig:scanip}
\end{figure}