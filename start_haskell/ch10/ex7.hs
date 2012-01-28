import Test.HUnit

data Expr = Val Int | Add Expr Expr | Mul Expr Expr

data Op = EVALA Expr | EVALM Expr | ADD Int | MUL Int
type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add e1 e2) c = eval e1 (EVALA e2 : c)
eval (Mul e1 e2) c = eval e1 (EVALM e2 : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALA e : c) n = eval e (ADD n : c)
exec (EVALM e : c) n = eval e (MUL n : c)
exec (ADD m : c) n = exec c (m + n)
exec (MUL m : c) n = exec c (m * n)

value :: Expr -> Int
value e = eval e []

valueTests :: [Test]
valueTests = map TestCase
  [ assertEqual "test1" 9 (value (Add (Add (Val 2) (Val 3) ) (Val 4) ) )
    -- 1 + 3 * 5 + 3 * 2 -> 46
   ,assertEqual "test2" 46 (value (Mul (Add (Mul (Add (Val 1) (Val 3) ) (Val 5) ) (Val 3) ) (Val 2)) )
  ]

runTests :: [Test] -> IO Counts
runTests ts = runTestTT $ TestList ts