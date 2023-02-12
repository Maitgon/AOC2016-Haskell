module Day06 where
import Data.List (transpose, maximumBy, minimumBy)
import qualified Data.MultiSet as MS
import Data.Function (on)

solve :: IO ()
solve = do
    
    -- Input reading.
    content <- readFile "inputs/input06.txt"
    let input = (transpose . lines) content

    -- Part 1.
    let sol1 = part1 input

    -- Part 2.
    let sol2 = part2 input

    putStrLn $ "Part 1 sol: " ++ show sol1
    putStrLn $ "Part 2 sol: " ++ show sol2 

part1 :: [[Char]] -> String
part1 = map (fst. maximumBy (compare `on` snd) . MS.toOccurList . MS.fromList)

part2 :: [[Char]] -> String
part2 = map (fst. minimumBy (compare `on` snd) . MS.toOccurList . MS.fromList)