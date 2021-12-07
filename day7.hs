import qualified Data.ByteString.Char8 as B
import Data.List
import System.Environment   

main = do
  (f:_) <- getArgs :: IO [String]
  xs <- fmap (\l -> (head l, length l)) . group . sort . parse <$> B.readFile f
  print $ solve p1 xs
  print $ solve p2 xs

solve p xs = minimum $ fmap (\n -> sum $ fmap (p n) xs) [0..length xs]

p1 n (p,c) = c*abs (n-p)
p2 n (p,c) = let d = abs (n-p) in c*d*(d+1) `div` 2

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
