-- With deep debt to Chris Penner, based on a modifed version of his code
-- here: https://chrispenner.ca/posts/conways-game-of-life
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}

import Data.Functor.Compose (Compose(..))
import qualified Data.Vector as V
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Arrow ((***),(&&&))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad (Comonad(..))

import System.Environment
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')

newtype VBounded a = VBounded (V.Vector a)
  deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
  distribute = distributeRep

gridSize :: Int
gridSize = 300

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v V.! ((i `min` gridSize - 1) `max` 0)
  tabulate desc = VBounded $ V.generate gridSize desc

type Grid a = Store (Compose VBounded VBounded) a
type Coord = (Int, Int)

mkGrid :: V.Vector (V.Vector Bool) -> Grid Bool
mkGrid vs = store lookup (0, 0)
  where
    lookup (x,y) = case vs V.!? (y-50) of
                     Just v -> case v V.!? (x-50) of
                                 Just b -> b
                                 _ -> False
                     _ -> False

type Rule = Grid Bool -> Bool

mkRule :: V.Vector Bool -> Rule
mkRule bs g =
  (bs V.! neighbourhood)
  where
    localNeighbourhood (i,j) = [(x, y) | y <- [j-1..j+1], x <- [i-1..i+1]]
    neighbourhood = b2d $ experiment (\s -> localNeighbourhood s) g

b2d bs = foldl' (\acc bit -> if bit then acc*2 + 1 else acc*2) 0 bs

step :: Rule -> Grid Bool -> Grid Bool
step = extend

render :: Grid Bool -> B.ByteString
render (StoreT (Identity (Compose g)) _) = B.pack $ foldMap ((++ "\n") . foldMap (bool "." "#")) g

parseFile = (head &&& (V.fromList . drop 2))
                  . fmap (V.fromList . fmap (== '#') . B.unpack)
                  . B.lines

main :: IO ()
main = do
  (f:_) <- getArgs :: IO [String]
  (rule,image) <- parseFile <$> B.readFile f
  print $ V.length rule
  let initial = mkGrid image
  let r = mkRule rule
      g = (flip (!!) 2) $ iterate (step r) initial
      g' = (flip (!!) (50-2)) $ iterate (step r) g
  B.putStrLn (render g)
  print . B.foldl' (\acc c -> bool acc (acc+1) (c == '#')) 0 $ render g
  B.putStrLn (render g')
  print . B.foldl' (\acc c -> bool acc (acc+1) (c == '#')) 0 $ render g'
