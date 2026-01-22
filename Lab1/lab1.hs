-- TASK 1: Add 4 to the end of the list [1,2,3]
-- This task demonstrates how to append an element to a list in Haskell.
task1 :: [Int]
task1 = [1,2,3] ++ [4]  -- Using the list concatenation operator (++)

-- TASK 2: Define at least 2 equivalent definitions for the `last` function
-- This task shows different ways to implement the `last` function.
-- Definition 1: Using recursion
last1 :: [a] -> a
last1 [x] = x
last1 (_:xs) = last1 xs

-- Definition 2: Using the `reverse` function
last2 :: [a] -> a
last2 xs = head (reverse xs)

-- TASK 3: Define at least 2 equivalent definitions for the `init` function
-- This task shows different ways to implement the `init` function.
-- Definition 1: Using recursion
init1 :: [a] -> [a]
init1 [_] = []
init1 (x:xs) = x : init1 xs

-- Definition 2: Using the `reverse` function
init2 :: [a] -> [a]
init2 xs = reverse (tail (reverse xs))

-- TASK 4: Define a function `fib` that returns the nth Fibonacci number
-- This task demonstrates how to calculate Fibonacci numbers recursively.
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- TASK 5: Define a function `fibonacci` that returns the first n Fibonacci numbers
-- This task demonstrates how to generate a list of Fibonacci numbers.
fibonacci :: Int -> [Int]
fibonacci n = map fib [0..(n-1)]  -- Using `map` to apply `fib` to the range [0..n-1]

-- Main function to demonstrate the tasks
main :: IO ()
main = do
    putStrLn "TASK 1: Add 4 to the end of the list [1,2,3]"
    print task1

    putStrLn "TASK 2: Two definitions of `last`"
    print $ last1 [1,2,3]
    print $ last2 [1,2,3]

    putStrLn "TASK 3: Two definitions of `init`"
    print $ init1 [1,2,3]
    print $ init2 [1,2,3]

    putStrLn "TASK 4: Fibonacci number at position 5"
    print $ fib 5

    putStrLn "TASK 5: First 10 Fibonacci numbers"
    print $ fibonacci 10