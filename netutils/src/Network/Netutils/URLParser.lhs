
\section{URL Parser}

\begin{code}
module Network.Netutils.URLParser
       ( URL(..)
       , parsingURL
       , parsingURLS
       , transToBS
       ) where

import Data.ByteString as B
import Data.ByteString.Char8 as B
import Text.Parsec
import Text.Parsec.Char
\end{code}


\begin{code}
data URL a = URL { urlProtocal :: !a
                 , urlHostName :: !a
                 , urlPort     :: Maybe a
                 , urlPath     :: !a
                 }
\end{code}


\begin{code}
parsingURLS :: Stream s => Parsec s u (URL String)
parsingURLS = do
  protocal <- many (noneOf ":/") <* string "://"
  hostname <- many (noneOf ":/")
  next <- oneOf ":/"
  port <- case next of
    ":" -> Just <$> many digit <* char '/'
    "/" -> return Nothing
  path <- many anyChar
  return $ URL protocal hostname port path
\end{code}

\begin{code}
transToBS :: URL String -> URL B.ByteString
transToBS URL{..} = URL (B.pack urlProtocal) (B.pack urlHostName) (B.pack urlPort) (B.pack urlPath)
\end{code}

\begin{code}
parsingURL :: B.ByteString -> Either B.ByteString (URL B.ByteString)
parsingURL url = case rt of
  Left e -> Left $ B.pack $ show e
  Right u -> Right $ transToBS url
  where rt = parse parsingURLS "" url
\end{code}

