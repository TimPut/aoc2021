main = do
  xs <- fmap read . lines <$> readFile "./day1input.txt" :: IO [Int]
  putStr "Part one: "
  print $ length . filter (>0) $ zipWith (-) (tail xs) (xs)
  let windowed = zipWith (+) (tail $ tail xs) (zipWith (+) (tail xs) xs) 
  putStr "Part two: "
  print $ length . filter (>0) $ zipWith (-) (tail windowed) (windowed)
