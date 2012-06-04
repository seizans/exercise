data Move = OneTwo
          | OneThree
          | TwoOne
          | TwoThree
          | ThreeOne
          | ThreeTwo
          deriving Show

hanoi :: Int -> Move -> [Move]
hanoi 1 move = [move]
hanoi n OneThree = hanoi (n-1) OneTwo   ++ [OneThree] ++ hanoi (n-1) TwoThree
hanoi n OneTwo   = hanoi (n-1) OneThree ++ [OneTwo]   ++ hanoi (n-1) ThreeTwo
hanoi n ThreeTwo = hanoi (n-1) ThreeOne ++ [ThreeTwo] ++ hanoi (n-1) OneTwo
hanoi n ThreeOne = hanoi (n-1) ThreeTwo ++ [ThreeOne] ++ hanoi (n-1) TwoOne
hanoi n TwoOne   = hanoi (n-1) TwoThree ++ [TwoOne]   ++ hanoi (n-1) ThreeOne
hanoi n TwoThree = hanoi (n-1) TwoOne   ++ [TwoThree] ++ hanoi (n-1) OneThree

hanoiMain :: Int -> [Move]
hanoiMain n | n < 1 = error "n must be a positive value"
            | otherwise = hanoi n OneThree
