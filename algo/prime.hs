import System.Environment (getArgs)

prime :: [Int]
prime = help [2..]
  where
    help ys = let y = head ys in y : help (sieve y ys)

sieve :: Int -> [Int] -> [Int]
sieve n xs = filter (\m -> m `mod` n /= 0) xs

main :: IO ()
main = do
    args <- getArgs
    let num = read $ head args :: Int
    print $ take num prime
