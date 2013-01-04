import Control.Applicative
import System.Environment (getArgs)

main :: IO ()
main = do
    str <- head <$> getArgs
    print $ isAllUnique str

isAllUnique :: Eq a => [a] -> Bool
isAllUnique [] = True
isAllUnique (c:cs) = if c `elem` cs then False else isAllUnique cs

-- below is substituted by elem
-- contains :: Char -> String -> Bool
-- contains c = any (==c)
