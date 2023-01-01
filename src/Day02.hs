{-# LANGUAGE OverloadedStrings #-}

module Day02 (solve) where

import Data.Time ( diffUTCTime, getCurrentTime )

data Direction = L | R | U | D
    deriving (Show, Eq)

data Pos = P Int Int

solve :: IO ()
solve = do
    start <- getCurrentTime

    -- Input reading.
    content <- readFile "inputs/input02.txt"
    let input = map parse $ lines content
    endParse <- getCurrentTime

    -- Part 1 solution
    let sol1 = part1 input (P 1 1)
    endPart1 <- getCurrentTime

    -- Part 2 solution
    let sol2 = part2 input (P 0 (-2))
    endPart2 <- getCurrentTime

    let timeParse = diffUTCTime endParse start
    let timePart1 = diffUTCTime endPart1 endParse
    let timePart2 = diffUTCTime endPart2 endPart1

    putStrLn $ "Time to parse: " ++ show timeParse
    putStrLn $ "Part 1 sol: " ++ show sol1 ++ " Time: " ++ show timePart1
    putStrLn $ "Part 2 sol: " ++ show sol2 ++ " Time: " ++ show timePart2
    putStrLn $ "Total time: " ++ show (timeParse + timePart1 + timePart2)

parse :: String -> [Direction]
parse [] = []
parse ('D':xs) = D : parse xs
parse ('L':xs) = L : parse xs
parse ('R':xs) = R : parse xs
parse ('U':xs) = U : parse xs
parse (_:_) = error "Input erronea"

part1 :: [[Direction]] -> Pos -> String
part1 [] _ = []
part1 (d:xs) initPos = head ((\(P x y) -> show $ 3*x + y + 1) newPos) : part1 xs newPos
    where newPos = getPos d initPos nextPos1

getPos :: [Direction] -> Pos -> (Pos -> Direction -> Pos) -> Pos
getPos ds p nextPos = foldl nextPos p ds

nextPos1 :: Pos -> Direction -> Pos
nextPos1 (P x y) R = if y >= 2 then P x y else P x (y+1)
nextPos1 (P x y) L = if y <= 0 then P x y else P x (y-1)
nextPos1 (P x y) D = if x >= 2 then P x y else P (x+1) y
nextPos1 (P x y) U = if x <= 0 then P x y else P (x-1) y

part2 :: [[Direction]] -> Pos -> String
part2 [] _ = []
part2 (d:xs) initPos = newPad newPos : part2 xs newPos
    where newPos = getPos d initPos nextPos2

nextPos2 :: Pos -> Direction -> Pos
nextPos2 (P x y) d = if abs nx + abs ny >= 3 then P x y else new
    where new@(P nx ny) = case d of
                     R -> P x (y+1)
                     L -> P x (y-1)
                     D -> P (x+1) y
                     U -> P (x-1) y

newPad :: Pos -> Char
newPad (P (-2) 0) = '1'
newPad (P (-1) (-1)) = '2'
newPad (P (-1) 0) = '3'
newPad (P (-1) 1) = '4'
newPad (P 0 (-2)) = '5'
newPad (P 0 (-1)) = '6'
newPad (P 0 0) = '7'
newPad (P 0 1) = '8'
newPad (P 0 2) = '9'
newPad (P 1 (-1)) = 'A'
newPad (P 1 0) = 'B'
newPad (P 1 1) = 'C'
newPad (P 2 0) = 'D'
newPad _ = error "Not in the pad"

