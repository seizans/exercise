{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Curl where

import Control.Applicative
import Data.Conduit
import Data.Conduit.Binary
import Network.HTTP.Conduit
import System.IO

import Data.ByteString
import Data.Attoparsec.ByteString
import Data.Conduit.Attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as AC

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Failure (Failure)

import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans (liftIO)

import Control.Exception.Lifted as E

field :: Parser ByteString
field = AC.takeWhile (/= ',') <* AC.anyChar

takeField :: (Monad m, MonadThrow m)
          => GLSink ByteString m ByteString
takeField = sinkParser field

pastmain :: IO ()
pastmain = do
    request <- parseUrl "http://localhost:8080/"
    withManager $ \manager -> do
        response <- http request manager
        responseBody response $$+- sinkHandle stdout

curl :: (MonadUnsafeIO m
        ,MonadThrow m
        ,MonadIO m
        ,MonadBaseControl IO m
        ,Failure HttpException m
        )
     => String -> m (ResumableSource (ResourceT m) ByteString)
curl url = do
    request <- parseUrl url
    withManager $ \manager ->
        responseBody <$> http request manager

getIntList :: (MonadBaseControl IO m, MonadThrow m)
           => ResumableSource m ByteString -> m [Int]
getIntList src = E.handle handler $ do
    (src1, str1) <- src $$++ takeField
    let v = read $ BC.unpack str1
    (v:) <$> getIntList src1
  where
    handler :: MonadBaseControl IO m => ParseError -> m [a]
    handler _ = return []

main :: IO ()
main = runResourceT $ do
    src0 <- liftIO $ curl "http://localhost:8080/"
    ss <- getIntList src0
    liftIO $ print ss
