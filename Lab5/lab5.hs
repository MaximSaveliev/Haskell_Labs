import Data.Char (chr, ord)

-- TASK 1: Define a function `pythagoreans` to calculate Pythagorean triplets
-- This task demonstrates how to use list comprehension to find Pythagorean triplets.
pythagoreans :: Int -> [(Int, Int, Int)]
pythagoreans limit = [(x, y, z) | x <- [1..limit], y <- [1..limit], z <- [1..limit], x^2 + y^2 == z^2]

-- TASK 2: Define a function `perfects` to find all perfect numbers up to a limit
-- This task demonstrates how to use list comprehension to find perfect numbers.
perfects :: Int -> [Int]
perfects limit = [n | n <- [1..limit], sum (init (divisors n)) == n]
  where
    divisors n = [x | x <- [1..n], n `mod` x == 0]  -- Find all divisors of n

-- TASK 3: Implement Caesar cipher with `encode` and `decode` functions
-- This task demonstrates how to use `chr` and `ord` to shift characters.
encode :: Int -> String -> String
encode shift = map (\c -> chr (ord 'a' + (ord c - ord 'a' + shift) `mod` 26))

decode :: Int -> String -> String
decode shift = encode (-shift)  -- Decoding is encoding with the negative shift

-- Main function to demonstrate the tasks
main :: IO ()
main = do
    putStrLn "TASK 1: Pythagorean triplets"
    print $ pythagoreans 10  -- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

    putStrLn "\nTASK 2: Perfect numbers"
    print $ perfects 500  -- [6,28,496]

    putStrLn "\nTASK 3: Caesar cipher"
    let message = "haskell"
    let shift = 3
    let encoded = encode shift message
    let decoded = decode shift encoded
    putStrLn $ "Original: " ++ message
    putStrLn $ "Encoded: " ++ encoded
    putStrLn $ "Decoded: " ++ decoded