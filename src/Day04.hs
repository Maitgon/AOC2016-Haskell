module Day04 where

import Data.Time ( diffUTCTime, getCurrentTime )
import Text.Regex.PCRE ( (=~), AllTextMatches(getAllTextMatches) )
import qualified Data.Map as M
import Data.List ( isPrefixOf, sortBy )

data Room = R [String] Int String
    deriving(Show)

getId :: Room -> Int
getId (R _ i _) = i

solve :: IO ()
solve = do
    start <- getCurrentTime

    -- Input reading.
    content <- readFile "inputs/input04.txt"
    let input = map parse $ lines content
    endParse <- getCurrentTime

    -- Part 1.
    let sol1 = sum $ map (\r@(R _ sectorId _) -> if isValid r then sectorId else 0) input
    endPart1 <- getCurrentTime

    -- Part 2. 
    let sol2 = getId $ head $ filter (\r -> "northpoleobject" `isPrefixOf` decrypt r ) $ filter isValid input -- Northpoleobjects doesnt work lmao
    endPart2 <- getCurrentTime

    let timeParse = diffUTCTime endParse start
    let timePart1 = diffUTCTime endPart1 endParse
    let timePart2 = diffUTCTime endPart2 endPart1

    putStrLn $ "Time to parse: " ++ show timeParse
    putStrLn $ "Part 1 sol: " ++ show sol1 ++ " Time: " ++ show timePart1
    putStrLn $ "Part 2 sol: " ++ show sol2 ++ " Time: " ++ show timePart2
    putStrLn $ "Total time: " ++ show (timeParse + timePart1 + timePart2)

parse :: String -> Room
parse str = R codes sectorId checksum
    where re = "([a-z]+)|([0-9]+)"
          matches = getAllTextMatches $ str =~ re :: [String]
          codes = take (length matches - 2) matches
          sectorId = (read :: String -> Int) (matches !! (length matches - 2))
          checksum = last matches

isValid :: Room -> Bool
isValid (R codes _ checksum) = checksum == take 5 (map fst vals')
    where vals = getVals $ concat codes
          vals' = sortBy compareSnd (M.assocs vals)
          compareSnd x y = compare (snd y) (snd x)

getVals :: String -> M.Map Char Int
getVals = foldr (\ c -> M.insertWith (+) c 1) M.empty

decrypt :: Room -> String
decrypt (R codes sectorId _) = concatMap aux codes
    where aux = map aux2
          aux2 c = ([c..'z'] ++ ['a'..]) !! (sectorId `mod` 26)


