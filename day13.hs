import Data.List.Split
import Data.List
import System.Environment   
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B

data Point = P Int Int
  deriving (Show,Eq,Ord)

data Fold = X Int | Y Int
  deriving (Show,Eq,Ord)

main = do
  (f:_) <- getArgs :: IO [String]
  file <- B.readFile f
  let ps = S.fromList $ fmap (\[a,b] -> P a b) . chunksOf 2 . parse $ file
      fs = fmap (parseFold . B.unpack . flip (!!) 2 . B.words) . drop (S.size ps + 1) . B.lines $ file
  print . S.size . fold ps . pure $ head fs
  putStrLn . unlines . pretty 40 $ fold ps fs

parseFold ('x':_:n) = X (read n)
parseFold ('y':_:n) = Y (read n)

fold :: S.Set Point -> [Fold] -> S.Set Point
fold ps fs = foldl' f ps fs
  where
    f ps (X n) = S.map (\(P x y) -> P (if x < n then x else n-(x-n)) y) ps               
    f ps (Y n) = S.map (\(P x y) -> P x (if y < n then y else n-(y-n))) ps

pretty bounds ps = fmap (\ps' -> [ c | i <- [0..bounds]
                                    , let P _ j = head ps'
                                    , let c = if P i j `elem` ps' then '#' else ' '
                                    ])
                   ls
  where
    ls = groupOn (\(P a b) -> b) . sortOn (\(P a b) -> b) $ S.toList ps

groupOn f = groupBy (\a b -> f a == f b)

parse = unfoldr go
  where
    go s = do (n, s1) <- B.readInt s
              let s2 = B.drop 1 s1
              return (n, s2)
    {-# INLINE go #-}
{-# INLINE parse #-}
