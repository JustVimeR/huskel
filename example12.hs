import System.IO

countPositiveNumsAndTheirPositions :: [Int] -> (Int, [Int])
countPositiveNumsAndTheirPositions xs = (length positiveNums, map fst filteredPairs)
  where
    indexedList = zip [1..] xs
    filteredPairs = filter (\(_, x) -> x > 0) indexedList
    positiveNums = map snd filteredPairs

main :: IO ()
main = do
  contents <- readFile "input12.txt"
  let nums = map read $ words $ map (\c -> if c == ',' then ' ' else c) contents :: [Int]
  let (count, positions) = countPositiveNumsAndTheirPositions nums
  putStrLn $ "Count of numbers with plus: " ++ show count
  putStrLn $ "Positions of plus numbers: " ++ show positions

