
isLowPoint :: (Ord a) => a -> a -> a -> Bool
isLowPoint left current right = current < left && current < right

splitByLowPoints :: (Ord a) => [a] -> [[a]]
splitByLowPoints [] = []
splitByLowPoints [x] = [[x]]
splitByLowPoints list = go [] list
  where
    go acc [] = [acc]
    go acc [x] = [acc ++ [x]]
    go acc (x:y:zs)
      | zs == [] = [acc ++ [x,y]]
      | isLowPoint x y (head zs) = (acc ++ [x,y]) : go [] (zs)
      | otherwise = go (acc ++ [x]) (y:zs)

readListFromFile :: FilePath -> IO [Int]
readListFromFile filePath = do
  content <- readFile filePath
  let numbers = map read $ words content :: [Int]
  return numbers

main :: IO ()
main = do
  numbers <- readListFromFile "input2.txt"
  print $ splitByLowPoints numbers
