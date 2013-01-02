import System.Environment (getArgs)

prime :: [Int]
prime = sieve [2..]

sieve :: [Int] -> [Int]
sieve (x:xs) = x : sieve (filter (\m -> m `mod` x /= 0) xs)
--sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

main :: IO ()
main = do
    args <- getArgs
    let num = read $ head args :: Int
    print $ take num prime
