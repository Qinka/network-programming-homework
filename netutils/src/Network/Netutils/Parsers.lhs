
\section{URL Parser}

\begin{code}
module Network.Netutils.Parsers
       ( URL(..)
       , parsingURL
       , parsingURLS
       , transToBS
       , getBodyLength
       
       ) where
import Data.ByteString (ByteString)
import qualified Data.ByteString as B hiding(pack,unpack)
import qualified Data.ByteString.Char8 as B
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


\begin{code}
parsingFTPLOGINC :: (Monad m, Stream s m Char)
                 => Parsec s u Bool
parsingFTPLOGINC = string "331" >> return True
\end{code}

\begin{code}
getFTPLOGINC :: B.ByteString -> Bool
getFTPLOGINC str = case parse parsingFTPLOGINC "" str of
  Right True -> True
  _ -> False
\end{code}

\begin{code}
parsingFTPLOGINS :: (Monad m, Stream s m Char)
                 => Parsec s u Bool
parsingFTPLOGINS = string "230" >> return True
\end{code}

\begin{code}
getFTPLOGINS ::B.ByteString -> Bool
getFTPLOGINS str = case parse parsingFTPLOGINS "" str of
  Right True -> True
  _ -> False
\end{code}

\begin{code}
parsingFTPPASV :: (Monad m, Stream s m Char)
               => Parsec s u (ByteString,ByteString)
parsingFTPPASV = do
  string "227"
  skipMany1 (noneOf "(,)")
  ip1:ip2:ip3:ip4:port1':port2':_ <- bewteen (char '(') (char ')') $ sepBy1 (many1 digit) (char ',')
  let host = BL.toStrict $ toLazyByteString $ foldr (\a b -> a `mappend` bytString b) mempt
        [ B.pack ip1,".",B.pack ip2,".",B.pack ip3,".",B.pack ip4]
      port1 = read port1
      port2 = read port2
      port = B.unpack $ show $ port1*256+port2
  return (host,port)
\end{code}


\begin{code}
getFTPPASV :: ByteString -> Maybe (ByteString,ByteString)
getFTPPASV str = case parse parsingFTPPASV "" str of
  Right x -> Just x
  Left _ -> Nothing
\end{code}
