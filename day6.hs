import qualified Data.ByteString.Char8 as B
import Data.List
import System.Environment   

main = do
  (f:_) <- getArgs :: IO [String]
  xs <- fmap (\l -> (head l, length l)) . group . sort . parse <$> B.readFile f
  print $ sum . fmap snd . (flip (!!) 80) $ iterate (reduce . concat . fmap step) xs
  print $ sum . fmap snd . (flip (!!) 256) $ iterate (reduce . concat . fmap step) xs

reduce xs = foldr f [] (sort xs)
  where
    f (a,n) ((b,m):xs) = if a == b then (b,m+n):xs else ((a,n):(b,m):xs)
    f (a,n) [] = [(a,n)]

step (d,n) = case d of
               0 -> [(6,n),(8,n)]
               _ -> [(d-1,n)]

unsafeReadInt bs = let Just parse = B.readInt bs in fst parse
{-# INLINE unsafeReadInt #-}

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
