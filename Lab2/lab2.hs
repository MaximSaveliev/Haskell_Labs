-- TASK 1: Determine the types of the following data
-- This task involves identifying the types of given Haskell expressions.

-- Type of [’a’, ’b’, ’c’]: [Char] (a list of characters, which is equivalent to a String)
-- Type of (’a’, ’b’, ’c’): (Char, Char, Char) (a tuple of three characters)
-- Type of [(False, ’O’), (True, ’1’)]: [(Bool, Char)] (a list of tuples, each containing a Bool and a Char)
-- Type of ([False, True], [’0’, ’1’]): ([Bool], [Char]) (a tuple of two lists, one of Bool and one of Char)
-- Type of [tail, init, reverse]: [[a] -> [a]] (a list of functions, each taking a list and returning a list)

-- TASK 2: Define a function equivalent to `length` using `map`
-- This task demonstrates how to use `map` to implement the `length` function.
lengthUsingMap :: [a] -> Int
lengthUsingMap xs = sum (map (\_ -> 1) xs)  -- Map each element to 1, then sum the list

-- Define a function `mapAll` that applies a list of functions to a list of elements
mapAll :: [a -> b] -> [a] -> [[b]]
mapAll fs xs = map (\f -> map f xs) fs  -- Apply each function in the list to all elements of xs

-- TASK 3: Define a function to test if an integer is a palindrome
-- This task involves checking if a number reads the same backward as forward.
isPalindrome :: Int -> Bool
isPalindrome n = let str = show n in str == reverse str  -- Convert to string and compare with its reverse

-- TASK 4: Define a function `block` to split a list into blocks of n elements
-- This task demonstrates how to partition a list into sublists of a given size.
block :: Int -> [a] -> [[a]]
block _ [] = []
block n xs = take n xs : block n (drop n xs)  -- Take n elements and recursively process the rest

-- TASK 5: Calculate the number of moves required for the Tower of Hanoi
-- This task involves calculating the minimum number of moves to solve the Tower of Hanoi puzzle.
hanoiMoves :: Int -> Int
hanoiMoves 0 = 0
hanoiMoves n = 2 * hanoiMoves (n - 1) + 1  -- Recursive formula: 2^(n) - 1

-- Example usage in the main function
main :: IO ()
main = do
    putStrLn "TASK 1: Types of given data"
    putStrLn "[’a’, ’b’, ’c’] :: [Char]"
    putStrLn "(’a’, ’b’, ’c’) :: (Char, Char, Char)"
    putStrLn "[(False, ’O’), (True, ’1’)] :: [(Bool, Char)]"
    putStrLn "([False, True], [’0’, ’1’]) :: ([Bool], [Char])"
    putStrLn "[tail, init, reverse] :: [[a] -> [a]]"

    putStrLn "\nTASK 2: Equivalent definition of `length` using `map`"
    print $ lengthUsingMap [1, 2, 3, 4, 5]

    putStrLn "\nTASK 2: Function `mapAll` example"
    print $ mapAll [(+1), (*2)] [1, 2, 3]

    putStrLn "\nTASK 3: Check if a number is a palindrome"
    print $ isPalindrome 121  -- True
    print $ isPalindrome 123  -- False

    putStrLn "\nTASK 4: Split a list into blocks of n elements"
    print $ block 3 "hello clouds"  -- ["hel", "lo ", "clo", "uds"]

    putStrLn "\nTASK 5: Calculate the number of moves for Tower of Hanoi"
    print $ hanoiMoves 3  -- 7 moves for 3 disks