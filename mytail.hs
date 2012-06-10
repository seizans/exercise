{-
 - 方針
 -  Sourceでは、seekして後ろから4096バイトずつ(又はファイルサイズの1/10バイトずつ)取得する。
 -  Conduitで、後ろからn個目の改行がある所までで入力を切る。
 -  Sinkは後ろから順に積んでいく。
 -}

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
--import Data.Conduit ( ($$), ($=) )
--import qualified Data.Conduit as C
import Data.Conduit
import System.Environment (getArgs)
import qualified System.IO as SI
import Data.Monoid

type FileSize = Integer

readLen :: FileSize -> Integer
readLen s
    | s < std  = std
    | otherwise = min limit x
  where
    x = s `div` 10
    std = 4096
    limit = 10 * 1024 * 1024

reverseFile :: MonadResource m => FilePath -> Source m BS.ByteString
reverseFile fp = sourceIO initialize close pull
  where
    initialize = do
        h <- SI.openBinaryFile fp SI.ReadMode
        s <- readLen <$> SI.hFileSize h
        b <- SI.hIsSeekable h
        if b
          then do
            SI.hSeek h SI.SeekFromEnd (-1)
            return $ Just (h, s)
          else return Nothing
    close st
        | Nothing    <- st = return ()
        | Just (h,_) <- st = SI.hClose h
    pull st
        | Nothing    <- st = return IOClosed
        | Just (h,l) <- st = do
            pos <- liftIO $ SI.hTell h
            if pos == 0
              then return IOClosed
              else do
                let len = min pos l
                liftIO $ SI.hSeek h SI.RelativeSeek (-len)
                x <- liftIO $ BS.hGetSome h $ fromIntegral len
                liftIO $ SI.hSeek h SI.RelativeSeek (-len)
                return $ IOOpen x

stackSink :: Monad m => Sink BS.ByteString m BS.ByteString
stackSink = sinkState BS.empty push close
  where
    close = return
    push st i = return (StateProcessing $ i `mappend` st)

limitLinesReverse :: Monad m => Int -> Conduit BS.ByteString m BS.ByteString
limitLinesReverse count = conduitState count push close
  where
    close _ = return []
    push n i
        | n == 0    = return (StateFinished (Just i) [])
        | otherwise =  do
            let (n',bs) = loop n i
            if n' == 0
                then return (StateFinished Nothing [bs])
                else return (StateProducing n' [bs])
    loop n x
        | c <= n = (n - c, x)
        | c > n  = (0, BC.drop (pos - 1) x)
      where
        c = BC.count '\n' x
        pos = BC.elemIndices '\n' x !! (c - n)

main :: IO ()
main = do
    [fp, n] <- getArgs
    x <- runResourceT $ reverseFile fp $= limitLinesReverse (read n) $$ stackSink
    BC.putStrLn $ BC.drop 1 x
