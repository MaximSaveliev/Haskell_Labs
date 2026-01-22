import System.IO
import Data.List (sort)
import Text.Read (readMaybe)

-- 1. IO action to concatenate two files whose names are read from the keyboard
concatFiles :: IO ()
concatFiles = do
    putStrLn "Enter first file name:"
    file1 <- getLine
    putStrLn "Enter second file name:"
    file2 <- getLine
    putStrLn "Enter output file name:"
    outFile <- getLine
    content1 <- readFile file1
    content2 <- readFile file2
    writeFile outFile (content1 ++ content2)
    putStrLn $ "Files concatenated into " ++ outFile

-- 2. IO action to read integers from successive lines until a non-integer is entered, then print sorted sequence
readSort :: IO ()
readSort = do
    putStrLn "Enter integers (one per line). Enter a non-integer to finish:"
    nums <- readInts []
    putStrLn "Sorted numbers:"
    print (sort nums)
  where
    readInts acc = do
      line <- getLine
      case readMaybe line :: Maybe Int of
        Just n  -> readInts (n:acc)
        Nothing -> return (reverse acc)

-- 3. Optional: X and O game using Graphics.SOE (not implemented here)
-- You can use Graphics.SOE.Gtk for a graphical implementation if desired.
-- main :: IO ()
-- main = ...

main :: IO ()
main = do
    putStrLn "Choose action:"
    putStrLn "1. Concatenate files"
    putStrLn "2. Read and sort integers"
    putStrLn "Enter 1 or 2:"
    choice <- getLine
    case choice of
      "1" -> concatFiles
      "2" -> readSort
      _   -> putStrLn "Invalid choice."
