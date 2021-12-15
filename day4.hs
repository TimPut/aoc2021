import qualified Data.ByteString.Char8 as B
import Data.List
import Data.List.Split
import qualified Data.Set as S
import Data.Maybe
import System.Environment   

main = do
  (f:_) <- getArgs :: IO [String]
  (ns:xs) <- B.lines <$> B.readFile f
  let nums = parse ns
      boardSize = length . B.words $ (xs !! 2)
      rows = fmap (fmap unsafeReadInt) . fmap B.words . tail <$> chunksOf (boardSize+1) xs
      cols = (fmap transpose rows)
      ls = zipWith (++) rows cols
      lineWinTimes = fmap (fmap (winTime nums)) ls :: [[Int]]
      boardWinTimes = fmap minimum $ lineWinTimes
      
      (firstTime,firstBoard) = minimum . flip zip [0..] $ boardWinTimes
      leftover = filter (\e -> not $ elem e (take firstTime nums)) (concat (ls !! (firstBoard)))

      (lastTime,lastBoard) = maximum . flip zip [0..] $ boardWinTimes
      leftover' = filter (\e -> not $ elem e (take lastTime nums)) (concat (ls !! (lastBoard)))

  print boardSize
  print $ (sum leftover `div` 2)*(nums !! (firstTime-1))
  print $ (sum leftover' `div` 2)*(nums !! (lastTime-1))

winTime nums r = fromJust $ findIndex (\n -> S.fromList r `S.isSubsetOf` n) nums'
  where
    nums' = scanl' (flip S.insert) S.empty nums

unsafeReadInt bs = let Just parse = B.readInt bs in fst parse
{-# INLINE unsafeReadInt #-}

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
