-- TASK 1: Define a function `powers` that generates a list of powers of a number
-- This task demonstrates how to create an infinite list of powers of a number.
powers :: Num a => a -> [a]
powers n = [n^x | x <- [1..]]  -- Generate powers of n starting from n^1

-- TASK 2: Define a function `longest` to find the longest words in a sentence
-- This task demonstrates how to process strings and find the longest words.
longest :: String -> [String]
longest sentence = filter ((== maxLength) . length) wordsList
  where
    wordsList = words sentence  -- Split the sentence into words
    maxLength = maximum (map length wordsList)  -- Find the maximum word length

-- TASK 3: Define a function to find the intersection of two lists
-- This task demonstrates how to find common elements between two lists.
intersection :: Eq a => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, x `elem` ys]  -- Keep elements in xs that are in ys

-- TASK 4: Define functions to convert a list of Bool to an integer and vice versa
-- This task demonstrates how to work with binary representations.
boolToInt :: [Bool] -> Int
boolToInt = foldl (\acc x -> acc * 2 + if x then 1 else 0) 0  -- Convert binary to integer

intToBool :: Int -> [Bool]
intToBool 0 = []
intToBool n = reverse (helper n)
  where
    helper 0 = []
    helper x = (x `mod` 2 == 1) : helper (x `div` 2)  -- Convert integer to binary

-- TASK 5: Define a function to calculate the greatest common divisor (GCD)
-- This task demonstrates how to implement the Euclidean algorithm.
gcd' :: Int -> Int -> Int
gcd' a 0 = abs a  -- Base case: GCD of a and 0 is a
gcd' a b = gcd' b (a `mod` b)  -- Recursive case

-- TASK 6: Implement a sorting algorithm (QuickSort)
-- This task demonstrates how to implement the QuickSort algorithm.
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]

-- TASK 7: Define a function to evaluate postfix expressions
-- This task demonstrates how to evaluate postfix expressions using a stack.
evalPostfix :: String -> Int
evalPostfix expr = head (foldl eval [] (words expr))
  where
    eval (x:y:ys) "+" = (y + x) : ys
    eval (x:y:ys) "-" = (y - x) : ys
    eval (x:y:ys) "*" = (y * x) : ys
    eval (x:y:ys) "/" = (y `div` x) : ys
    eval stack num = read num : stack  -- Push numbers onto the stack

-- Main function to demonstrate the tasks
main :: IO ()
main = do
    putStrLn "TASK 1: Powers of 2"
    print $ take 10 (powers 2)  -- Take the first 10 powers of 2

    putStrLn "\nTASK 2: Longest words in a sentence"
    print $ longest "examples of functional algorithms"  -- ["functional", "algorithms"]

    putStrLn "\nTASK 3: Intersection of two lists"
    print $ intersection [1, 2, 3, 4] [3, 4, 5, 6]  -- [3, 4]

    putStrLn "\nTASK 4: Convert Bool list to Int and back"
    print $ boolToInt [True, True, False]  -- 6
    print $ intToBool 6  -- [True, True, False]

    putStrLn "\nTASK 5: Greatest Common Divisor"
    print $ gcd' 56 98  -- 14

    putStrLn "\nTASK 6: QuickSort implementation"
    print $ quickSort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]  -- [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]

    putStrLn "\nTASK 7: Evaluate postfix expressions"
    print $ evalPostfix "1 2 3 + *"  -- 1 * (2 + 3) = 5
    print $ evalPostfix "2 3 * 1 +"  -- 1 + (2 * 3) = 7
    print $ evalPostfix "1 3 + 2 *"  -- (1 + 3) * 2 = 8