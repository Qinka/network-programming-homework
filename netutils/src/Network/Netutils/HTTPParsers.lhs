
\section{URL Parser}

\begin{code}
module Network.Netutils.HTTPParsers
       ( URL(..)
       , parsingURL
       , parsingURLS
       , transToBS
       , getBodyLength
       ) where

import Data.ByteString as B hiding(pack,unpack)
import Data.ByteString.Char8 as B
import Data.Functor.Identity
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
parsingURLS :: Stream s Identity Char => Parsec s u (URL String)
parsingURLS = do
  protocal <- many (noneOf ":/") <* string "://"
  hostname <- many (noneOf ":/")
  next <- option '/' $ oneOf ":/"
  port <- case next of
    ':' -> Just <$> many digit <* char '/'
    '/' -> return Nothing
  path <- many anyChar
  return $ URL protocal hostname port ('/':path)
\end{code}

\begin{code}
transToBS :: URL String -> URL B.ByteString
transToBS URL{..} = URL (B.pack urlProtocal) (B.pack urlHostName) (B.pack <$> urlPort) (B.pack urlPath)
\end{code}

\begin{code}
parsingURL :: B.ByteString -> Either B.ByteString (URL B.ByteString)
parsingURL url = case rt of
  Left e -> Left $ B.pack $ show e
  Right u -> Right $ transToBS u
  where rt = parse parsingURLS "" url
\end{code}


\begin{code}
parsingBodyLength :: Stream s Identity Char => Parsec s u Int
parsingBodyLength = try line <|> retry
  where line = do
          string "Content-Length: "
          num <- many1 digit
          return $ read num
        retry = do
           many1 (noneOf "\r\n") <* endOfLine
           parsingBodyLength
\end{code}

\begin{code}
getBodyLength :: B.ByteString -> Int
getBodyLength httpHead = case rt of
  Left _ -> 0
  Right i -> i 
  where rt = parse parsingBodyLength "" httpHead
\end{code}
