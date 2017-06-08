
\subsection{Executable}
\label{sec:oc:executable}

\begin{code}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment

import qualified Network.Netutils.FTP      as FTP
import qualified Network.Netutils.POP      as POP
import qualified Network.Netutils.ScanIP   as SIP
import qualified Network.Netutils.ScanPort as SPT
import qualified Network.Netutils.WWW      as WWW
\end{code}

In this section, the code is for the program, the \lstinline|Main| module.

\begin{code}
main :: IO ()
main = do
  let is = [ www, ftp, pop, scanip, scanport ]
  mkMain is  
\end{code}


\begin{code}
www :: Item
www = Item { itemMainMethod = WWW.main
           , itemName       = "netutils-www"
           , itemInfo       = "Test the http-get"
           }
ftp :: Item
ftp = Item { itemMainMethod = FTP.main
           , itemName       = "netutils-ftp"
           , itemInfo       = "Test the ftp and LIST command"
           }
pop :: Item
pop = Item { itemMainMethod = POP.main
           , itemName       = "netutils-pop"
           , itemInfo       = "Test the pop and its commands"
           }
scanip :: Item
scanip = Item { itemMainMethod = SIP.main
              , itemName       = "netutils-scanip"
              , itemInfo       = "Scan the ip"
              }
scanport :: Item
scanport = Item { itemMainMethod = SPT.main
                , itemName       = "netutils-scanport"
                , itemInfo       = "Scan the port"
                }
\end{code}




\begin{code}
data Item = Item { itemMainMethod :: IO ()
                 , itemName       :: String
                 , itemInfo       :: String
                 }
\end{code}

\begin{code}
mkMain :: [Item] -> IO ()
mkMain is = do
  let im = mkMI is
  name <- getProgName
  case name `itemSelect` im of
    Just m -> m
    Nothing -> do
      let pri Item{..} = do
            putStrLn $ "\n" ++ itemName
            putStrLn           itemInfo
      mapM_ pri is

mkMI :: [Item] -> Map String Item
mkMI = Map.fromList . map mk
  where mk i@Item{..} = (itemName,i)

itemSelect :: String -> Map String Item -> Maybe (IO ())
itemSelect name im = (fmap get) $ Map.lookupIndex (takeWhile (/='.') name) im
  where get i = itemMainMethod $ snd $ Map.elemAt i im
  
\end{code}
