\begin{code}

module Main where
import System.Environment
import Network.Netutils.WWW as WWW

main :: IO ()
main = getProgName >>= run
  where run name = case name of
          "netutils-www.exe" -> WWW.wwwMain
        
          _ -> putStrLn $ "??" ++ name
          
          
  
\end{code}
