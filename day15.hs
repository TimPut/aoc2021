import Data.List
import Data.List.Split
import Data.Maybe
import System.Environment   
import qualified Data.ByteString.Char8 as B
import qualified Data.Heap as H
import qualified Data.Map as M
import qualified Data.Set as S

data Point = P Int Int
  deriving (Eq, Show, Ord)
data Bounds = B Int Int
  deriving (Show)
data Grid = Grid (M.Map Point Int) Bounds
  deriving (Show)

main = do
  (f:_) <- getArgs :: IO [String]
  file <- B.readFile f
  let (x,y) = (\ls -> (B.length . head $ ls, length ls)) . B.lines $ file
      ns = (fmap (read . pure)) . B.unpack . B.concat . B.lines $ file :: [Int]
      m = M.fromList $ flip zip ns [P i j | i <- [0..x-1]
                                          , j <- [0..y-1]]
      g = Grid m (B x y)
      scale = 5
      bigG = tileRight scale $ tileDown scale g
  -- putStrLn $ pretty bigG
  print $ dijkstra g M.! P (x-1) (y-1)
  print $ dijkstra bigG M.! P (x*scale-1) (y*scale-1)  

pretty (Grid m (B x y)) = unlines
                          . chunksOf y
                          $ [head $ show (m M.! P i j) | i <- [0..x-1]
                                                       , j <- [0..y-1]]

tileRight n (Grid m (B x y)) = Grid m' (B (x*n) y)
  where
    m' = M.unions
         . take n
         . iterate (M.map (\r -> (r `mod` 9) + 1)
                    . M.mapKeys (\(P i j) -> P (i+x) j))
         $ m
tileDown n (Grid m (B x y)) = Grid m' (B x (y*n))
  where
    m' = M.unions
         . take n
         . iterate (M.map (\r -> (r `mod` 9) + 1)                    
                   . M.mapKeys (\(P i j) -> P i (j+y)))
         $ m

dijkstra :: Grid -> M.Map Point Int
dijkstra (Grid rs bounds) = go (H.singleton (H.Entry (0 :: Int) (P 0 0))) M.empty S.empty
  where
    lookup = M.findWithDefault maxBound
    go queue dists visited = case H.viewMin queue of
                               Just (H.Entry k v,queue') ->
                                 if (v `S.notMember` visited) && (k <= v `lookup` dists)
                                 then let ns = neighbors bounds v
                                          inserts = catMaybes $
                                            fmap (\p -> let dp = k + p `lookup` rs
                                                       in
                                                         if p `lookup` dists < dp
                                                         then Nothing
                                                         else Just (H.Entry dp p)) ns
                                          queue'' = foldl' (flip H.insert) queue' inserts
                                          dists' = foldl' (\m (H.Entry p v) -> M.insert v p m) dists inserts
                                 in go queue'' dists' (v `S.insert` visited)
                                 else go queue' dists (v `S.insert` visited)
                               Nothing -> dists


                                               
neighbors :: Bounds -> Point -> [Point]
neighbors (B x y) (P i j) = [P i' j' | (i',j') <- [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
                                 , i' `from` (0,x)
                                 , j' `from` (0,y)]
  where
    from i (a,b) = a <= i && i < b 
