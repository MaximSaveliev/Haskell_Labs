-- TASK 1: Define `safeTail` using different approaches
-- This task demonstrates how to define a safe version of `tail`.

-- Using a conditional expression
safeTailCond :: [a] -> [a]
safeTailCond xs = if null xs then [] else tail xs

-- Using guarded equations
safeTailGuard :: [a] -> [a]
safeTailGuard xs
  | null xs   = []
  | otherwise = tail xs

-- Using pattern matching
safeTailPattern :: [a] -> [a]
safeTailPattern []     = []
safeTailPattern (_:xs) = xs

-- TASK 2: Define a function `subList` to check if a list is included in another
-- This task demonstrates how to check if all elements of one list are in another.
subList :: Eq a => [a] -> [a] -> Bool
subList [] _ = True  -- An empty list is always a sublist
subList (x:xs) ys = x `elem` ys && subList xs ys  -- Check if head is in ys and recurse

-- TASK 3: Define a function to calculate the product of two polynomials
-- This task demonstrates polynomial multiplication using coefficient lists.
polyProduct :: Num a => [a] -> [a] -> [a]
polyProduct xs ys = foldl addPoly (replicate (length xs + length ys - 1) 0) [shift i (map (*x) ys) | (x, i) <- zip xs [0..]]
  where
    shift n zs = replicate n 0 ++ zs  -- Shift the polynomial by n places
    addPoly p1 p2 = zipWith (+) (pad p1 len) (pad p2 len)  -- Add two polynomials
      where
        len = max (length p1) (length p2)
    pad zs n = zs ++ replicate (n - length zs) 0  -- Pad the list with zeros to match length

-- TASK 4: Define a function `pascal` to return the nth row of Pascal's triangle
-- This task demonstrates how to generate rows of Pascal's triangle.
pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = zipWith (+) ([0] ++ prevRow) (prevRow ++ [0])
  where
    prevRow = pascal (n - 1)

-- Main function to demonstrate the tasks
main :: IO ()
main = do
    putStrLn "TASK 1: Define `safeTail` using different approaches"
    print $ safeTailCond [1, 2, 3]  -- [2, 3]
    print (safeTailCond ([] :: [Int]))         -- []
    print $ safeTailGuard [1, 2, 3] -- [2, 3]
    print (safeTailGuard ([] :: [Int]))        -- []
    print $ safeTailPattern [1, 2, 3] -- [2, 3]
    print (safeTailPattern ([] :: [Int]))        -- []

    putStrLn "\nTASK 2: Check if a list is a sublist of another"
    print $ subList [1, 2] [1, 2, 3, 4]  -- True
    print $ subList [1, 5] [1, 2, 3, 4]  -- False

    putStrLn "\nTASK 3: Polynomial product"
    print $ polyProduct [1, 2] [3, 4]  -- [3, 10, 8]
    print $ polyProduct [1, 0, 2] [1, 2]  -- [1, 2, 2, 4]

    putStrLn "\nTASK 4: Pascal's triangle row"
    print $ pascal 0  -- [1]
    print $ pascal 1  -- [1, 1]
    print $ pascal 4  -- [1, 4, 6, 4, 1]