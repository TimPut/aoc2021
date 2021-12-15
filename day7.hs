import qualified Data.ByteString.Char8 as B
import Data.List
import System.Environment   

main = do
  (f:_) <- getArgs :: IO [String]
  xs <- fmap (\l -> (head l, length l)) . group . sort . parse <$> B.readFile f
  xs' <- parse <$> B.readFile f
  print $ mean xs'
  print $ median xs'
  print $ solve' (median xs') p1 (sort xs)
  print $ solve' (mean xs') p2 xs
  print $ solve p1 xs
  print $ solve p2 xs

mean xs = let y = floor $ (\(acc,n) -> fromIntegral acc / fromIntegral n) $foldl' f (0,0) xs
          in [y,y+1]
  where
    f (acc,n) val = (acc+val,n+1) 

median xs = let x = floor $ fromIntegral (length xs) / 2 in [x,x+1]

solve p xs = minimum $ fmap (\n -> sum $ fmap (p n) xs) [0..length xs]

solve' ys p xs = minimum $ fmap (\n -> sum $ fmap (p n) xs) ys

p1 n (p,c) = c*abs (n-p)
p2 n (p,c) = let d = abs (n-p) in c*d*(d+1) `div` 2

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
