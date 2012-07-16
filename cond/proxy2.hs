{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Text (encode, decode, utf8)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

import Data.Text (toUpper)
import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)

import Control.Concurrent.Async (race_)
import Control.Monad.Trans (liftIO)
import Network (withSocketsDo)

main :: IO ()
main = withSocketsDo $ do
  runTCPServer (ServerSettings 5123 HostAny) server
  `race_` runTCPServer (ServerSettings 6123 HostAny) proxy
  `race_` runTCPClient (ClientSettings 6123 "localhost") client

server :: Application IO
server src sink = src $$ decode utf8 =$ CL.map toUpper =$ encode utf8 =$ sink

takeLine :: Sink ByteString IO ByteString
takeLine = do
  let linefeed = 10
  bss <- CB.takeWhile (/= linefeed) =$ CL.consume
  CB.drop 1 -- drop the newline
  return $ S8.takeWhile (/= '\r') $ S8.concat bss

getPortHost :: Sink ByteString IO ClientSettings
getPortHost = do
  portBS <- takeLine
  hostBS <- takeLine
  return $ ClientSettings (read $ S8.unpack portBS) (S8.unpack hostBS)

proxy :: Application IO
proxy fromClient0 toClient = do
  (fromClient, clientSettings) <- fromClient0 $$+ getPortHost
  runTCPClient clientSettings $ \fromServer toServer -> do
    yield "Connected to server" $$ toClient
    (fromServer $$ toClient) `race_` (fromClient $$+- toServer)

client :: Application IO
client src sink = src $$ conduit =$ sink
  where
    conduit = do
      yield "4000\r\n"
      yield "localhost\r\n"
      await >>= liftIO . print

      yield "hello"
      await >>= liftIO . print

      yield "world"
      await >>= liftIO . print

      yield "goodbye"
      await >>= liftIO . print
