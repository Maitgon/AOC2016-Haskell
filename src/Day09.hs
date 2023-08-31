module Day09 where

data Marker = M Int Int

solve :: IO ()
solve = do
    content <- readFile "inputs/input09.txt"
    let input = filter (/= '\n') content

    -- Part 1
    let sol1 = part1 input

    -- Part 2
    let sol2 = part2 input

    -- Print solutions
    putStrLn $ "Part 1 sol: " ++ show sol1
    putStrLn $ "Part 2 sol: " ++ show sol2

-- Part 1

part1 :: String -> Int
part1 [] = 0
part1 ('(':xs) = let (M n1 n2, xs') = parseMarker xs
                 in (n1*n2) + part1 (drop n1 xs')
part1 (_:xs) = 1 + part1 xs

parseMarker :: String -> (Marker, String)
parseMarker str = (M (read n1) (read n2), drop 1 rest')
    where (n1, rest) = break (== 'x') str
          (n2, rest') = break (== ')') (drop 1 rest)

-- Part 2
part2 :: String -> Int
part2 [] = 0
part2 ('(':xs) = let (M n1 n2, xs') = parseMarker xs
                 in n2 * part2 (take n1 xs') + part2 (drop n1 xs')
part2 (_:xs) = 1 + part2 xs

