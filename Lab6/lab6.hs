import Data.List (find, sort, permutations)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

-- TASK 1: Define a function to calculate e^x using the Maclaurin series
-- This task demonstrates how to use `zipWith` and `takeWhile` to calculate a series.
maclaurinExp :: Double -> Double
maclaurinExp x = sum $ takeWhile (> 1e-6) terms
  where
    terms = zipWith (/) (iterate (*x) 1) (scanl (*) 1 [1..])

-- TASK 2: Define a function `digits` to find the number of digits in the smallest multiple of n
-- This task demonstrates how to work with numbers in base 10.
digits :: Integer -> Integer
digits n = findDigits 1 1
  where
    findDigits m len
      | m `mod` n == 0 = len
      | otherwise      = findDigits ((m * 10 + 1) `mod` n) (len + 1)

-- TASK 3: Define a function to solve the sequence problem
-- This task demonstrates how to find the maximum sequence length for a range of numbers.
solve :: Int -> Int -> (Int, Int, Int)
solve i j = (i, j, maximum $ map collatzLength [i..j])
  where
    collatzLength n = length (takeWhile (/= 1) (iterate collatz n)) + 1
    collatz n
      | even n    = n `div` 2
      | otherwise = 3 * n + 1

-- TASK 4: Define a function `isJumper` to check if a list is a jumper
-- This task demonstrates how to check differences between successive elements.
isJumper :: [Int] -> Bool
isJumper xs
  | length xs <= 1 = True
  | otherwise = sort (map absDiff (zip xs (tail xs))) == [1..length xs - 1]
  where
    absDiff (a, b) = abs (a - b)

data Op = Add | Sub | Mul | Div deriving (Eq, Show)

applyOp :: Op -> Int -> Int -> Maybe Int
applyOp Add x y = Just (x + y)
applyOp Sub x y = Just (x - y)
applyOp Mul x y = Just (x * y)
applyOp Div x y = if y /= 0 && x `mod` y == 0 then Just (x `div` y) else Nothing

opStr :: Op -> String
opStr Add = "+"
opStr Sub = "-"
opStr Mul = "*"
opStr Div = "/"

-- Expression tree
data Expr = Val Int | App Op Expr Expr

-- Evaluate expression tree
neval :: Expr -> Maybe Int
neval (Val n) = Just n
neval (App op l r) = do
  x <- neval l
  y <- neval r
  applyOp op x y

-- Convert expression tree to string
nshowExpr :: Expr -> String
nshowExpr (Val n) = show n
nshowExpr (App op l r) = "(" ++ nshowExpr l ++ opStr op ++ nshowExpr r ++ ")"

-- All ways to split a list into two non-empty parts
splits :: [a] -> [([a],[a])]
splits [] = []
splits [_] = []
splits xs = [(take i xs, drop i xs) | i <- [1..length xs-1]]

-- All possible expression trees from a list of numbers (in order)
exprs :: [Int] -> [Expr]
exprs [n] = [Val n]
exprs ns = [App op l r | (ls, rs) <- splits ns
                        , l <- exprs ls
                        , r <- exprs rs
                        , op <- [Add, Sub, Mul, Div]]

-- Try all possible groupings and operator combinations, including all permutations
solveExpression :: [Int] -> Int -> String
solveExpression nums target =
  let results = [nshowExpr e | ns <- permutations nums, e <- exprs ns, neval e == Just target]
  in if null results then "" else head (sort results)

-- Main function to demonstrate the tasks
main :: IO ()
main = do
    putStrLn "TASK 1: Maclaurin series for e^x"
    print $ maclaurinExp 1  -- Approximation of e^1

    putStrLn "\nTASK 2: Smallest multiple with only digit 1"
    print $ digits 9901  -- 12
    print $ digits 3     -- 3
    print $ digits 7     -- 6

    putStrLn "\nTASK 3: Collatz sequence solver"
    print $ solve 1 10      -- (1,10,20)
    print $ solve 100 200   -- (100,200,125)
    print $ solve 201 210   -- (201,210,89)
    print $ solve 900 1000  -- (900,1000,174)

    putStrLn "\nTASK 4: Jumper list checker"
    print $ isJumper [1,4,2,3]       -- True
    print $ isJumper [1,4,2,-1,6]   -- False

    putStrLn "\nTASK 5: Solve mathematical expression"
    print $ solveExpression [2,4,5,9,75] 658  -- "(2*(9+(4*(5+75))))"
    print $ solveExpression [9,1,4,8] 26      -- ""
    print $ solveExpression [9,1,4,8] (-17)   -- "(1-((9*8)/4))"