type Pos = (Int, Int)

data Cell = Free
          | Reached
          | Queen
          deriving Show

type Board = [[Cell]]

newBoard :: Int -> Board
newBoard n | n < 4 = error "n must be more than 3"
           | otherwise = [xs | xs <- [newRow n | _ <- [1..n] ] ]
  where
    newRow n = [Free | _ <- [1..n] ]

putQueen :: Pos -> Board -> Board
putQueen (x,y) board | cell (x,y) board == Free = putQueen' (x,y) board
                     | otherwise = error "cannot put Queen there"
  where
    cell (x',y') board' = board' !! y' !! x'
    putQueen' (x,y) (bs:bss) = 

nqueenMain :: Int -> [Pos]
nqueenMain n | n < 4 = error "n must be more than 3"
             | otherwise = nqueen n n

nqueen :: Int -> Int -> [Pos]
nqueen 1 1 = [(1,1)]
