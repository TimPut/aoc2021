import qualified Data.ByteString.Char8 as B
import Data.List
import System.Environment   

main = do
  (f:_) <- getArgs :: IO [String]
  ls <- B.lines <$> B.readFile f
  let displays = fmap (fmap B.unpack . B.words) . B.split '|' <$> ls
  print $ length $ filter (\l -> length l `elem` [2,3,4,7]) . concat $ fmap last displays
  print . sum $ fmap solve displays 

solve [ds,us] = foldl1' (\acc x -> acc*10+x)
                . fmap (decodeDigit . wordCode ds)
                $ us

-- from digitCodes
decodeDigit :: Int -> Int
decodeDigit d = case d of
          1845900 -> 0
          1058400 -> 1
          1034460 -> 2
          1940400 -> 3
          1214640 -> 4 
          1819440 -> 5
          1820700 -> 6
          1625400 -> 7
          1971900 -> 8
          1970640 -> 9

digitCodes :: [Int]
digitCodes = fmap (\d -> wordCode valid (valid !! d)) [0..9]

wordCode :: [String] -> String -> Int
wordCode table w = sum (fmap (charCode table) w)

charCode :: [String] -> Char -> Int
charCode table c = product $ fmap length $ filter (elem c) table

valid = [['a','b','c','e','f','g'] -- 0
        ,['c','f'] -- 1
        ,['a','c','d','e','g'] -- 2
        ,['a','c','d','f','g'] -- 3
        ,['b','c','d','f'] -- 4
        ,['a','b','d','f','g'] -- 5        
        ,['a','b','d','e','f','g'] -- 6
        ,['a','c','f'] -- 7
        ,['a','b','c','d','e','f','g'] -- 8
        ,['a','b','c','d','f','g']] -- 9
