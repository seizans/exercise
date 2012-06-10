{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Monoid (mappend)

-- src に存在するファイル名を与えると dest で指定したファイル名にコピーする
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest = runResourceT $ CB.sourceFile src $$ CB.sinkFile dest

consumer :: Sink ByteString (ResourceT IO) ()
consumer = do
    mw <- CB.head
    case mw of
      Nothing -> return ()
      Just w  -> do
        liftIO . putStr $ "XXX "
        liftIO . BC.putStrLn . BS.singleton $ w
        consumer

consumer2 :: Sink ByteString (ResourceT IO) ()
consumer2 = do
    mw <- CB.head
    case mw of
      Nothing -> return ()
      Just w -> do
        liftIO . putStr $ "YYY "
        liftIO . BC.putStrLn . BS.singleton $ w

listFeeder :: Source (ResourceT IO) ByteString
listFeeder = CL.sourceList ["12", "34"]

fileFeeder :: Source (ResourceT IO) ByteString
fileFeeder = CB.sourceFile "File"

test1 :: IO ()
test1 = runResourceT $ listFeeder $$ consumer
test2 = runResourceT $ (listFeeder `mappend` fileFeeder) $$ consumer
test3 = runResourceT $ (listFeeder `mappend` fileFeeder) $$ (consumer2 >> consumer)
test4 = runResourceT $ listFeeder $$ CB.isolate 10 =$ consumer
test5 = runResourceT $ listFeeder $= CB.isolate 3 $$ consumer
