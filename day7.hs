import qualified Data.ByteString.Char8 as B
import Data.List
import System.Environment   

main = do
  (f:_) <- getArgs :: IO [String]
  xs <- fmap (\l -> (head l, length l)) . group . sort . parse <$> B.readFile f
  print $ minimum $ zip (fmap (cost xs) [0..length xs]) [0..]
  print $ minimum $ zip (fmap (cost' xs) [0..length xs]) [0..]

cost xs n = sum $ fmap (\(p,c) -> c*abs (n-p)) xs
cost' xs n = sum $ fmap (\(p,c) -> c*moveCost (abs (n-p))) xs

moveCost n = n*(n+1) `div` 2 -- sum [1..n]

unsafeReadInt bs = let Just parse = B.readInt bs in fst parse
{-# INLINE unsafeReadInt #-}

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
