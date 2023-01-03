module Day03 where

import Data.Time ( diffUTCTime, getCurrentTime )
import Text.Regex.PCRE ( (=~), AllTextMatches(getAllTextMatches) )
import Data.List ( transpose )

data Triangle = T Int Int Int
    deriving(Show)

solve :: IO ()
solve = do
    start <- getCurrentTime

    -- Input reading.
    content <- readFile "inputs/input03.txt"
    let input = map parse $ lines content
    endParse <- getCurrentTime

    -- Part 1.
    let sol1 = length $ filter isTriangle input
    endPart1 <- getCurrentTime

    -- Part 2.
    let input2 = map (\[x,y,z] -> T x y z) $ groupsOf 3 $ concat $ transpose $ map parseAux $ lines content
    let sol2 = length $ filter isTriangle input2
    endPart2 <- getCurrentTime

    let timeParse = diffUTCTime endParse start
    let timePart1 = diffUTCTime endPart1 endParse
    let timePart2 = diffUTCTime endPart2 endPart1

    putStrLn $ "Time to parse: " ++ show timeParse
    putStrLn $ "Part 1 sol: " ++ show sol1 ++ " Time: " ++ show timePart1
    putStrLn $ "Part 2 sol: " ++ show sol2 ++ " Time: " ++ show timePart2
    putStrLn $ "Total time: " ++ show (timeParse + timePart1 + timePart2)


parse :: String -> Triangle
parse str = T (head vals) (vals !! 1) (last vals)
    where re = "[0-9]+"
          vals = map (read :: String -> Int) $ getAllTextMatches $ str =~ re :: [Int]

parseAux :: String -> [Int]
parseAux str = map (read :: String -> Int) $ getAllTextMatches $ str =~ "[0-9]+" :: [Int]

groupsOf :: Int -> [a] -> [[a]]
groupsOf = go []
  where
    go acc _ [] = acc
    go acc n xs = go (before : acc) n after
      where
        (before, after) = splitAt n xs
        
isTriangle :: Triangle -> Bool
isTriangle (T x y z) = x < y + z && y < x + z && z < x + y
