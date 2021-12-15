import Data.List
import System.Environment   
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Control.Monad

main = do
  (f:_) <- getArgs :: IO [String]
  ls <- fmap B.unpack . B.lines <$> B.readFile f
  let h = head . head $ ls
      rules = M.fromList . fmap parseRule . tail . tail $ ls :: M.Map String Char
      polymer = M.fromListWith (+) $ zip (window . head $ ls) [1,1..]

  forM [10,40,4000,40000] $ \n -> print
    . (\cs -> last cs - head cs)
    . sort 
    . fmap snd
    . M.toList
    . M.insertWith (+) h 1
    . count
    . flip (!!) n $ iterate (step rules) polymer

count = M.foldlWithKey' (\m [a,b] c -> M.insertWith (+) b c $ m) M.empty

window :: String -> [String]
window str = zipWith (\a b -> [a,b]) str (tail str)

step :: M.Map String Char -> M.Map String Integer -> M.Map String Integer
step rules pairs = M.fromListWith (+)
                   . concat
                   . fmap (\(pair,count) -> applyRules rules pair count)
                   . M.toList
                   $ pairs

applyRules :: M.Map String Char -> String -> Integer -> [(String,Integer)]
applyRules rs [a,b] n = case rs M.!? [a,b] of
                      Just c -> [([a,c],n),([c,b],n)]
                      Nothing -> [([a,b],n)]  

parseRule str = (take 2 str, last str)
