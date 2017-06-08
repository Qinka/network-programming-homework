\section{File Transfer Protocol}
\label{sec:ftp}

\begin{code}
module Network.Netutils.FTP
       ( main
       ) where

import Network.Netutils.Socket
import Network.Netutils.FTP.GUI
\end{code}

In this section, there are the codes about FTP.

First of the following codes is about the main method, which encapsulated to the real main function.
Next one is the codes about the communicating with server.
Finally, the code remained is the GUI.

\begin{code}
main :: IO ()
main = do
  initGUI
  win <- mkFTPWindow :: IO (FTPWindow Inet)
  mkFieldCheckEvent win
  mkButtonsClickedEvent win
  displayAll win
\end{code}


\inputHaskell{src/Network/Netutils/FTP/Internal.lhs}
\inputHaskell{src/Network/Netutils/FTP/GUI.lhs}

\subsection{Application}
\label{sec:ftp:app}

The figure \ref{fig:ftp} is the snap shot of the sceen.

\begin{figure}[h]
  \centering
  \includegraphics[width=1\linewidth]{img/ftp}
  \caption{The sceen shot of the application}
  \label{fig:ftp}
\end{figure}
