-- X and O game (text-based)
-- This is a simple text-based version for demonstration. For a graphical version, install Graphics.SOE.Gtk and replace IO with graphical actions.

import Data.List (transpose)
import Control.Monad (when)

-- Board is a list of 9 cells (row-major)
type Board = [Char]

emptyBoard :: Board
emptyBoard = replicate 9 ' '

showBoard :: Board -> String
showBoard b = unlines [row r | r <- [0,3,6]]
  where row i = concat [cell (i+j) | j <- [0..2]]
        cell k = ' ' : [b !! k] ++ " "

move :: Board -> Int -> Char -> Maybe Board
move b i c
  | b !! i == ' ' = Just (take i b ++ [c] ++ drop (i+1) b)
  | otherwise     = Nothing

winner :: Board -> Maybe Char
winner b = checkLines (rows ++ cols ++ diags)
  where
    rows = [[b!!i, b!!(i+1), b!!(i+2)] | i <- [0,3,6]]
    cols = [[b!!i, b!!(i+3), b!!(i+6)] | i <- [0,1,2]]
    diags = [[b!!0, b!!4, b!!8], [b!!2, b!!4, b!!6]]
    checkLines = foldr (\l acc -> if all (/= ' ') l && all (== head l) l then Just (head l) else acc) Nothing

full :: Board -> Bool
full = all (/= ' ')

main :: IO ()
main = do
  putStrLn "X and O Game!"
  gameLoop emptyBoard 'X'

gameLoop :: Board -> Char -> IO ()
gameLoop b p = do
  putStrLn $ showBoard b
  case winner b of
    Just w  -> putStrLn $ "Player " ++ [w] ++ " wins!"
    Nothing -> if full b
                  then putStrLn "It's a draw!"
                  else do
                    putStrLn $ "Player " ++ [p] ++ ", enter position (0-8):"
                    inp <- getLine
                    case reads inp of
                      [(i,"")] | i >= 0 && i < 9 ->
                        case move b i p of
                          Just b' -> gameLoop b' (if p == 'X' then 'O' else 'X')
                          Nothing -> do putStrLn "Invalid move!"; gameLoop b p
                      _ -> do putStrLn "Invalid input!"; gameLoop b p
