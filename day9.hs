import qualified Data.ByteString.Char8 as B
import Data.List
import System.Environment   
import Data.List.Split
import Control.Arrow
import Data.Char
import Data.Graph
import Data.Ord
import Data.Tree

main = do
  (f:_) <- getArgs :: IO [String]
  file <- readFile f :: IO String
  let size = length . head . lines $ file
      ls = chunksOf size . fmap (first (read . pure)) . flip zip [0..] . filter isNumber $ file :: [[(Int,Int)]]
  print $ lowPoints2d ls
  let (g,v,k) = (mkGraph ls)
  print $ product . take 3 . sortOn Down . fmap length . fmap flatten $ components g

lowPoints1d [] = []
lowPoints1d [c] = [c]
lowPoints1d (x:y:xs) = if fst x < fst y then x : lowPoints1d' (x:y:xs) else lowPoints1d' (x:y:xs)

lowPoints1d' (l:c:r:xs) = if fst l > fst c && fst r > fst c then c : lowPoints1d' (c:r:xs) else lowPoints1d' (c:r:xs)
lowPoints1d' [l,c] = if fst l > fst c then pure c else lowPoints1d' []
lowPoints1d' _ = []


mkGraph rs = graphFromEdges (mkEdges rs)

mkEdges rs = fmap merge . groupOn snd' . sortOn snd' . concat . fmap mkRowEdges $ rs ++ transpose rs
  where
    snd' = (\(_,k,_) -> k)
    
merge ((n,k,ks):es) = (n,k,ks')
  where
    ks' = sort . concat . fmap (\(_,_,ks) -> ks) $ es

mkRowEdges ((a,b):(c,d):xs) = case (a /= 9,c /= 9) of
                                (True,True) -> [(b,b,[d]),(d,d,[b])] ++ mkRowEdges ((c,d):xs)
                                (_,_) -> [(b,b,[]),(d,d,[])] ++ mkRowEdges ((c,d):xs)

mkRowEdges _ = []


-- lowPoints2d :: [[(Int, Int)]] -> [[(Int, Int)]]
lowPoints2d rs = sum . fmap ((+) 1 . fst . head) . filter (\l -> 2 <= length l) . groupOn snd . sortOn snd . concat $ (cw ++ rw)
  where
    cw = fmap lowPoints1d $ transpose rs
    rw = fmap lowPoints1d rs

solve = undefined
solve' = undefined

groupOn f = groupBy (\a b -> f a == f b)

unsafeReadInt bs = let Just parse = B.readInt bs in fst parse
{-# INLINE unsafeReadInt #-}

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
