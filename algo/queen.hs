type Pos = (Int, Int)

isOutOfQueen :: Pos -> Pos -> Bool
isOutOfQueen (x, y) (qx, qy) | x == qx = False
                             | y == qy = False
                             | x + y == qx + qy = False
                             | x - y == qx - qy = False
                             | otherwise = True

canPut :: Pos -> [Pos] -> Bool
canPut pos queens = and $ map (isOutOfQueen pos) queens

qall :: Pos -> Int -> [Pos] -> [[Pos]]
qall p@(1,1) n qs | canPut p qs = [p : qs]
                  | otherwise = []
qall p@(1,y) n qs | canPut p qs = qall (n, y - 1) n (p : qs)
                  | otherwise = []
qall p@(x,1) n qs | canPut p qs = [p : qs] ++ qall (x - 1, 1) n qs
                  | otherwise = qall (x - 1, 1) n qs
qall p@(x,y) n qs | canPut p qs = qall (n, y - 1) n (p : qs) ++ qall (x - 1, y) n qs
                  | otherwise = qall (x - 1, y) n qs

queen :: Pos -> Int -> [Pos] -> [Pos]
queen p@(1,1) n qs | canPut p qs = p : qs
                   | otherwise = []
queen p@(1,y) n qs | canPut p qs = queen (n, y - 1) n (p : qs)
                   | otherwise = [] 
queen p@(x,1) n qs | canPut p qs = p : qs
                   | otherwise = queen (x - 1, 1) n qs
queen p@(x,y) n qs | canPut p qs = let qnext = queen (n, y - 1) n (p : qs) in if qnext /= []
                         then qnext
                         else queen (x - 1, y) n qs
                   | otherwise = queen (x - 1, y) n qs

--queen pos@(x,y) n queens | canPut pos queens = if y > 1
--                               then let queens2 = queen (n, y - 1) n (pos:queens) in (if queens2 == [] then queen (x-1, y) n queens else queens2)
--                               else (pos: queens)
--                         | x > 1 = queen (x - 1, y) n queens
--                         | x == 1 = []

queenMain :: Int -> [Pos]
queenMain n | n < 4 = error "n must be more than 3"
            | otherwise = queen (n, n) n []

qallMain :: Int -> [[Pos]]
qallMain n = qall (n, n) n []

test1 = canPut (1,1) [(2,3), (3,2)]
test2 = canPut (1,1) [(2,3), (1,5)]
test3 = canPut (1,1) [(2,3), (5,1)]
test4 = canPut (1,1) [(2,3), (5,5)]
test5 = canPut (2,3) [(1,1), (3,4)]
