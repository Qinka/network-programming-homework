
\section{URL Parser}

\begin{code}
module Network.Netutils.Parsers
       ( URL(..)
       , parsingURL
       , parsingURLS
       , transToBS
       , getBodyLength
       , getFTPR
       , getFTPPASV
       ) where
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString as B hiding(pack,unpack)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
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
parsingFTPPASV :: Stream s Identity Char
               => Parsec s u (ByteString,ByteString)
parsingFTPPASV = do
  skipMany1 (noneOf "(,)")
  ip1:ip2:ip3:ip4:port1':port2':_ <- between (char '(') (char ')') $ sepBy1 (many1 digit) (char ',')
  let host = BL.toStrict $ toLazyByteString $ foldr (\a b -> byteString a `mappend` b) mempty
        [ B.pack ip1,".",B.pack ip2,".",B.pack ip3,".",B.pack ip4]
      port1 = read port1'
      port2 = read port2'
      port = B.pack $ show $ port1*256+port2
  return (host,port)
\end{code}


\begin{code}
getFTPPASV :: ByteString -> (ByteString,ByteString)
getFTPPASV str = case parse parsingFTPPASV "" str of
  Right x -> x
  Left e -> error $ show e
\end{code}

\begin{code}
parsingFTPR  :: Stream s Identity Char
             => Parsec s u (Maybe Int,ByteString)
parsingFTPR = do
  state <- getState
  str <- nextString
  return (state,B.pack str)
  where nextString = (++"\r\n") <$> many (noneOf "\r\n") <* string "\r\n"
        getState = (read <$> many1 digit) >>=
          (\state -> try (char '-' >> return Nothing) <|> (char ' ' >> return (Just state)))
\end{code}

\begin{code}
getFTPR :: ByteString -> (Maybe Int,ByteString)
getFTPR str = case parse parsingFTPR "" str of
  Right x -> x
  Left e -> (Just (-1) ,B.pack $ show e)
\end{code}
