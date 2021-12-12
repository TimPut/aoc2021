import Data.List.Split
import System.Environment   
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Bool
import Data.Char
import Control.Monad

main = do
  (f:_) <- getArgs :: IO [String]
  file <- readFile f
  let edges = fmap (splitOn "-") . lines $ file
      edges' = fmap (\[a,b] -> (a,[b])) $ (fmap reverse edges) ++ edges
      graph = M.fromListWith (++) edges'
  print . length $ explore graph (S.singleton "start") "start"
  print . length $ explore' graph (S.singleton "start") "start"

explore :: M.Map String [String] -> S.Set String -> String -> [()]
explore g visited start = do
  step <- filter (not . (\s -> s `S.member` visited && small s)) $ g M.! start
  if step == "end" then pure () else explore g (step `S.insert` visited) step

explore' :: M.Map String [String] -> S.Set String -> String -> [()]
explore' g visited start = do
  step <- filter (/= "start") $ g M.! start
  if step `S.member` visited && small step then explore g visited step else
    if step == "end" then pure () else explore' g (step `S.insert` visited) step
 
small = isLower . head
