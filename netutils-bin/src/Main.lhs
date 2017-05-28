\begin{code}

module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory
import System.Environment

import qualified Network.Netutils.WWW as WWW

main :: IO ()
main = do
  let is = [ www ]
  mkMain is  
\end{code}


\begin{code}
www :: Item
www = Item { itemMainMethod = WWW.main
                , itemName       = "netutils-www"
                , itemInfo       = "Test the http-get"
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
  where mk i@Item{..} = (itemName++exeExtension,i)

itemSelect :: String -> Map String Item -> Maybe (IO ())
itemSelect name im = (fmap get) $ Map.lookupIndex name im
  where get i = itemMainMethod $ snd $ Map.elemAt i im
  
\end{code}
