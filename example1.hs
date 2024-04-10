import System.IO

filterPositionMatches :: [Int] -> [Int]
filterPositionMatches xs = [x | (x, i) <- zip xs [1..], x == i]

readIntsFromFile :: FilePath -> IO [Int]
readIntsFromFile fileName = do
  contents <- readFile fileName
  let ints = map read . words $ contents :: [Int]
  return ints

main :: IO ()
main = do
  intList <- readIntsFromFile "input1.txt"
  let result = filterPositionMatches intList
  print result
