{-# LANGUAGE OverloadedStrings #-}

module Day01 (solve) where

import Data.List.Split ( splitOn )
import Data.List (intersect)
import Data.Time ( diffUTCTime, getCurrentTime )


data Input = Inp Direction Int
    deriving Show

data Direction = L | R | U | D
    deriving (Show, Eq)

solve :: IO ()
solve = do
    start <- getCurrentTime

    -- Input reading.
    content <- readFile "inputs/input01.txt"
    let vector = map parse $ splitOn ", " content
    endParse <- getCurrentTime

    -- Part 1 solution
    let sol1 = part1 U (0, 0) vector
    endPart1 <- getCurrentTime

    -- Part 2 solution
    let sol2 = part2 U (0, 0) vector []
    endPart2 <- getCurrentTime

    let timeParse = diffUTCTime endParse start
    let timePart1 = diffUTCTime endPart1 endParse
    let timePart2 = diffUTCTime endPart2 endPart1

    --let totalTime = timeParse + timePart1 + timePart2
    putStrLn $ "Time to parse: " ++ show timeParse
    putStrLn $ "Part 1 sol: " ++ show sol1 ++ " Time: " ++ show timePart1
    putStrLn $ "Part 2 sol: " ++ show sol2 ++ " Time: " ++ show timePart2
    putStrLn $ "Total time: " ++ show (timeParse + timePart1 + timePart2)

parse :: String -> Input
parse [] = undefined
parse (s:str) = Inp dir n
    where dir = if s == 'R' then R else L
          n = read str :: Int

part1 :: Direction -> (Int, Int) -> [Input] -> Int
part1 _ (x, y) [] = manhattanDist x y
part1 dir (x, y) ((Inp d n):xs) = part1 newDir newPos xs
    where newDir = case dir of
                        U -> if d == L then L else R
                        L -> if d == L then D else U
                        D -> if d == L then R else L
                        R -> if d == L then U else D
          newPos = case newDir of
                        U -> (x, y+n)
                        L -> (x-n, y)
                        D -> (x, y-n)
                        R -> (x+n, y)

part2 :: Direction -> (Int, Int) -> [Input] -> [(Int, Int)] -> Int
part2 _ (x, y) [] _ = manhattanDist x y
part2 dir (x, y) ((Inp d n):xs) visited = if aux /= []
                                          then uncurry manhattanDist (head aux)
                                          else part2 newDir (last newPos) xs (newPos ++ visited)
    where newDir = case dir of
                        U -> if d == L then L else R
                        L -> if d == L then D else U
                        D -> if d == L then R else L
                        R -> if d == L then U else D
          newPos = case newDir of
                        U -> [(x, y+i) | i <- [1..n]]
                        L -> [(x-i, y) | i <- [1..n]]
                        D -> [(x, y-i) | i <- [1..n]]
                        R -> [(x+i, y) | i <- [1..n]]
          aux = newPos `intersect` visited


manhattanDist :: Int -> Int -> Int
manhattanDist x y = abs x + abs y
