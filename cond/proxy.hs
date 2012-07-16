{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Text (encode, decode, utf8)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

import Data.Text (toUpper)
import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
    _ <- forkIO $ runTCPServer (ServerSettings 4000 HostAny) server
    _ <- forkIO $ runTCPServer (ServerSettings 5000 HostAny) proxy
    runTCPClient (ClientSettings 5000 "127.0.0.1") client

server :: Application IO
server src sink = src $$ decode utf8 =$ CL.map toUpper =$ encode utf8 =$ sink

takeLine :: Sink ByteString IO ByteString
takeLine = do
    let linefeed = 10
    bss <- CB.takeWhile (/= linefeed) =$ CL.consume
    CB.drop 1
    return $ S8.takeWhile (/= '\r') $ S8.concat bss

getPortHost :: Sink ByteString IO ClientSettings
getPortHost = do
    portBS <- takeLine
    hostBS <- takeLine
    return $ ClientSettings (read $ S8.unpack portBS) (S8.unpack hostBS)

proxy :: Application IO
proxy fromClient0 toClient = do
    (fromClient, clientSettings) <- fromClient0 $$+ getPortHost
    runTCPClient clientSettings (proxyLoop fromClient toClient)

proxyLoop :: ResumableSource IO ByteString
          -> Sink ByteString IO ()
          -> Source IO ByteString
          -> Sink ByteString IO ()
          -> IO ()
proxyLoop fromClient0 toClient fromServer0 toServer = do
    yield "Connected to server" $$ toClient
    (fromServer, ()) <- fromServer0 $$+ return ()
    loop fromClient0 fromServer
  where
    loop fromClient fromServer = do
        (fromClient', mbsl) <- fromClient $$++ await
        case mbsl of
            Nothing -> close fromClient' fromServer
            Just bsl -> do
                yield bsl $$ toServer
                (fromServer', mbs2) <- fromServer $$++ await
                case mbs2 of
                    Nothing -> do
                        yield "Server closed connection" $$ toClient
                        close fromClient' fromServer'
                    Just bs2 -> do
                        yield bs2 $$ toClient
                        loop fromClient' fromServer'
    close x y = do
        x $$+- return ()
        y $$+- return ()

client :: Application IO
client src sink = src $$ conduit =$ sink
  where
    conduit = do
        yield "4000\r\n"
        yield "127.0.0.1\r\n"
        await >>= liftIO . print
        yield "hello"
        await >>= liftIO . print
        yield "world"
        await >>= liftIO . print
        yield "goodbye"
        await >>= liftIO . print

