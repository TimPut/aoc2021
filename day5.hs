import qualified Data.ByteString.Char8 as B
import Data.List

data Point = P Int Int
  deriving (Show, Eq, Ord)
type Line = [Point]

line (p@(P a b),q@(P c d))
  | horz p q = [P x b | x <- [min a c..max a c]]
  | vert p q = [P a y | y <- [min b d..max b d]]
  | diag p q = let xs = [min a c..max a c]
                   ys = [min b d..max b d]
                   xs' = if c > a then xs else reverse xs
                   ys' = if d > b then ys else reverse ys
               in [P x y | (x,y) <- zip xs' ys']
  | otherwise = []

vert (P x y) (P w z) = x == w
horz (P x y) (P w z) = y == z
diag (P x y) (P w z) = abs (x-w) == abs (y-z)

fluff c = case c of
            ',' -> True
            ' ' -> True
            '-' -> True
            '>' -> True
            otherwise -> False
main = do
  xs <- fmap (\[a,b,c,d] -> (P a b, P c d))
       . fmap (fmap unsafeReadInt)
       . fmap (filter (not . B.null))
       . fmap (B.splitWith fluff) . B.lines <$> B.readFile "./day5input.txt"
  let orthos = filter (\l -> uncurry horz l || uncurry vert l) xs
      diags = filter (\l -> uncurry diag l) xs
  print $ length . filter ((<) 1 . length) .  group . sort . concat $ fmap line orthos
  print $ length . filter ((<) 1 . length) .  group . sort . concat $ fmap line (diags ++ orthos)

unsafeReadInt bs = let Just parse = B.readInt bs in fst parse
{-# INLINE unsafeReadInt #-}
