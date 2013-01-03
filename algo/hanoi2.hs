import Control.Applicative
import System.Environment (getArgs)

data Move
    = OneTwo | OneThree
    | TwoOne | TwoThree
    | ThreeOne | ThreeTwo
  deriving (Show)

hanoi :: Int -> Move -> [Move]
hanoi 1 mv = [mv]
hanoi n mv
    = hanoi (n-1) (prv mv)
    ++ mv : (hanoi (n-1) (nxt mv) )

prv :: Move -> Move
prv OneTwo = OneThree
prv OneThree = OneTwo
prv TwoOne = TwoThree
prv TwoThree = TwoOne
prv ThreeOne = ThreeTwo
prv ThreeTwo = ThreeOne

nxt :: Move -> Move
nxt OneTwo = ThreeTwo
nxt OneThree = TwoThree
nxt TwoOne = ThreeOne
nxt TwoThree = OneThree
nxt ThreeOne = TwoOne
nxt ThreeTwo = OneTwo

main :: IO ()
main = do
    arg <- read <$> (head <$> getArgs)
    print $ hanoi arg OneThree
