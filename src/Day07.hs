module Day07 where

import Text.Regex.Applicative
import Data.Maybe ( fromJust )

data IpV7 = IpV7 [String] [String]
    deriving (Show)

solve :: IO ()
solve = do
    -- Input reading.
    content <- readFile "inputs/input07.txt"

    let input = parseInput content

    -- Part 1
    let sol1 = part1 input

    -- Part 2
    let sol2 = part2 input
    
    putStrLn $ "Part 1 sol: " ++ show sol1
    putStrLn $ "Part 2 sol: " ++ show sol2 

-- Parsing Input

parseInput :: String -> [IpV7]
parseInput content = map parseLine (lines content)

parseLine :: String -> IpV7
parseLine str = uncurry IpV7 (evenOdd lin)
    where
    lin = fromJust $ match readBoth str

    evenOdd :: [a] -> ([a], [a])
    evenOdd [] = ([], [])
    evenOdd [x] = ([x], [])
    evenOdd (x:y:xs) = ([x], [y]) <> evenOdd xs

readBoth :: RE Char [String]
readBoth = many (readThroughBracket <|> readUntilEndOfLine)

readUntilEndOfLine :: RE Char String
readUntilEndOfLine = many (psym (/= '\n'))

readThroughEndOfLine :: RE Char String
readThroughEndOfLine = readUntilEndOfLine <* sym '\n'

readUntilBracket :: RE Char String
readUntilBracket = many (psym (\c -> c /= '[' && c /= ']'))

readThroughBracket :: RE Char String
readThroughBracket = readUntilBracket <* (sym '[' <|> sym ']')

-- Solve problem
-- Part 1

abba :: String -> Bool
abba (x:y:z:c:xs) = x /= y && y == z && x == c || abba (y:z:c:xs)
abba _ = False

part1 :: [IpV7] -> Int
part1 input = length (filter supportsTLS input)
    where
    supportsTLS (IpV7 l1 l2) = any abba l1 && not (any abba l2)

-- Part 2

part2 :: [IpV7] -> Int
part2 input = length (filter supportsSSL input)
    where
    supportsSSL (IpV7 l1 l2) = any (isBab (concatMap getAba l1)) l2

    isBab l (x:y:z:xs) = x == z && [y,x,y] `elem` l || isBab l (y:z:xs)
    isBab _ _ = False

    getAba :: String -> [String]
    getAba (x:y:z:xs) = if x /= y && x == z
                    then [x,y,z] : getAba (y:z:xs)
                    else getAba (y:z:xs)
    getAba _ = []