import Control.Applicative
import System.Environment (getArgs)

type Pos = (Int, Int)

nqueen :: Int -> [[Pos]]
nqueen n
    | n <= 2    = error "n should be lte 3"
    | otherwise = concat [nqueen' n (i,n) [] | i <- [1..n] ]

nqueen' :: Int -> Pos -> [Pos] -> [[Pos]]
nqueen' n (x,1) ps
    | canPut (x,1) ps = [(x,1) : ps]
    | otherwise = []
nqueen' n (x,y) ps = if canPut (x,y) ps
    then concat [nqueen' n (i,(y-1)) ((x,y):ps) | i <- [1..n] ]
    else []

canPut :: Pos -> [Pos] -> Bool
canPut q [] = True
canPut q (p:ps) = if inArea p q then False else canPut q ps

inArea :: Pos -> Pos -> Bool
inArea (a,b) (x,y)
    | a == x    = True
    | b == y    = True
    | y-b == x-a = True
    | y-b == -x+a = True
    | otherwise = False

main :: IO ()
main = do
    n <- read <$> (head <$> getArgs)
    let ps = nqueen n
    mapM_ print ps
    print $ length ps
