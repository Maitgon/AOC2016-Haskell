{-# LANGUAGE OverloadedStrings #-}
module Day05 where

import Data.Time ( getCurrentTime, diffUTCTime )
import Data.ByteString ( ByteString )
import Crypto.Hash ( hash, Digest, MD5 )
import Data.String (fromString)
import Data.List (sortBy)
import Data.Function (on)

solve :: IO ()
solve = do
    start <- getCurrentTime

    -- Input reading.
    let input = "ugkcyxxp"
    endParse <- getCurrentTime

    -- Part 1.
    let sol1 = part1 input
    endPart1 <- getCurrentTime

    -- Part 1.
    let sol2 = part2 input
    endPart2 <- getCurrentTime

    let timeParse = diffUTCTime endParse start
    let timePart1 = diffUTCTime endPart1 endParse
    let timePart2 = diffUTCTime endPart2 endPart1

    putStrLn $ "Time to parse: " ++ show timeParse
    putStrLn $ "Part 1 sol: " ++ show sol1 ++ " Time: " ++ show timePart1
    putStrLn $ "Part 2 sol: " ++ show sol2 ++ " Time: " ++ show timePart2
    putStrLn $ "Total time: " ++ show (timeParse + timePart1 + timePart2)

hexMD5 :: String -> String
hexMD5 = show . (hash :: ByteString -> Digest MD5) . fromString

hashes :: String -> [String]
hashes input = map (hexMD5 . (input ++) . show) [(0 :: Integer) ..]

part1 :: String -> String
part1 input = (take 8 . map (!! 5) . filter want) h
  where h = hashes input
        want inp = take 5 inp == "00000"

part2 :: String -> String
part2 input = map snd (getFirst8 [] $ (take 100 . filter want) h)
  where h = hashes input
        want inp = take 5 inp == "00000"

getFirst8 :: [(Char, Char)] -> [String] -> [(Char, Char)]
getFirst8 _ [] = error "Not enough"
getFirst8 vals (x:xs)
  | length vals == 8 = sortBy (compare `on` fst) vals
  | not (checkKey k vals) && '0' <= k && k <= '7' = getFirst8 ((k, val):vals) xs
  | otherwise = getFirst8 vals xs
  where k = x !! 5
        val = x !! 6
        checkKey _ [] = False
        checkKey ke ((key, _):v) = ke == key || checkKey ke v