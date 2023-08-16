{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Day08 where

import Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import qualified Data.Set as S

data Instruction = Rect { x :: Int, y :: Int}
                 | RotateRow { y :: Int, r :: Int }
                 | RotateCol { x :: Int, r :: Int }
                 deriving (Show)

solve :: IO ()
solve = do
    -- Input reading.
    content <- readFile "inputs/input08.txt"

    let input = parseInput content

    -- Part 1
    let solveScreen = part1 (fromRight [] input)
    let sol1 = S.size solveScreen

    -- Part 2
    let sol2 = part2 solveScreen

    putStrLn $ "Part 1 sol: " ++ show sol1
    putStrLn $ "Part 2 sol:\n" ++ sol2

-- Part 1

type Screen = S.Set (Int, Int)

maxX :: Int
maxX = 50
maxY :: Int
maxY = 6

part1 :: [Instruction] -> Screen
part1 = foldl applyInstruction S.empty

applyInstruction :: Screen -> Instruction -> Screen
applyInstruction screen (Rect x y) = S.union screen (S.fromList [(x', y') | x' <- [0..x-1], y' <- [0..y-1]])
applyInstruction screen (RotateRow y r) = S.map (rotateRow' y r) screen
applyInstruction screen (RotateCol x r) = S.map (rotateCol' x r) screen

rotateRow' :: Int -> Int -> (Int, Int) -> (Int, Int)
rotateRow' y r (x, y') = if y == y' then ((x + r) `mod` maxX, y') else (x, y')

rotateCol' :: Int -> Int -> (Int, Int) -> (Int, Int)
rotateCol' x r (x', y) = if x == x' then (x', (y + r) `mod` maxY) else (x', y)

-- Part 2
part2 :: Screen -> String
part2 screen = unlines [row y | y <- [0..maxY-1]]
    where row y = [if S.member (x, y) screen then '#' else '.' | x <- [0..maxX-1]]



-- Parsing Input

parseInput :: String -> Either ParseError [Instruction]
parseInput = parse parseDay08 "(unknown)"

parseDay08 :: Parser [Instruction]
parseDay08 = endBy line eol

line :: Parser Instruction
line = try rect <|> try rotateRow <|> rotateCol

rect :: Parser Instruction
rect = do
    string "rect "
    x <- many1 digit
    char 'x'
    y <- many1 digit
    return $ Rect (read x) (read y)

--rect :: Parser Instruction
--rect = string "rect " >* Rect <$> (many1 digit <* char 'x') <*> many1 digit

rotateRow :: Parser Instruction
rotateRow = do
    string "rotate row y="
    y <- many1 digit
    string " by "
    r <- many1 digit
    return $ RotateRow (read y) (read r)

rotateCol :: Parser Instruction
rotateCol = do
    string "rotate column x="
    x <- many1 digit
    string " by "
    r <- many1 digit
    return $ RotateCol (read x) (read r)


eol = char '\n'

