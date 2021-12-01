{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import Data.Maybe

main = do
  xs <- fmap (fst . fromJust . B.readInt) . B.lines <$> B.readFile "./day1input.txt" :: IO [Int]

  B.putStr "Part one: "
  B.putStrLn $ B.pack . show . length . filter (>0) $ zipWith (-) (tail xs) xs

  let windowed = foldr1 (zipWith (+)) . take 3 $ iterate tail xs
  B.putStr "Part two: "
  B.putStrLn $ B.pack . show . length . filter (>0) $ zipWith (-) (tail windowed) windowed
