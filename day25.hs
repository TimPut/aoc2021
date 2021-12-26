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

newtype HBounded a = HBounded (V.Vector a)
  deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
  distribute = distributeRep

instance Distributive HBounded where
  distribute = distributeRep

gridSize :: Int
gridSize = 139

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v V.! (i `mod` (gridSize-0))
  tabulate desc = VBounded $ V.generate (gridSize-0) desc

instance Representable HBounded where
  type Rep HBounded = Int
  index (HBounded v) i = v V.! (i `mod` (gridSize-2))
  tabulate desc = HBounded $ V.generate (gridSize-2) desc

type Grid a = Store (Compose HBounded VBounded) a
type Coord = (Int, Int)

data Tern = Blank | South | East
  deriving (Eq,Show,Ord)

mkGrid :: V.Vector (V.Vector Tern) -> Grid Tern
mkGrid vs = store lookup (0, 0)
  where
    lookup (x,y) = case vs V.!? (x) of
                     Just v -> case v V.!? (y) of
                                 Just c -> c
                                 _ -> Blank
                     _ -> Blank

type Rule = Grid Tern -> Tern

eastRule :: Rule
eastRule g = case [left,center,right] of
               [East,Blank,_] -> East
               [_,East,Blank] -> Blank
               [_,c,_] -> c
  where
    localNeighbourhood (x,y) = [(x,y-1),(x,y),(x,y+1)]
    [left,center,right] = experiment (\s -> localNeighbourhood s) g

southRule :: Rule
southRule g = case [above,center,below] of
               [South,Blank,_] -> South
               [_,South,Blank] -> Blank
               [_,c,_] -> c
  where
    localNeighbourhood (x,y) = [(x-1,y),(x,y),(x+1,y)]
    [above,center,below] = experiment (\s -> localNeighbourhood s) g

step :: Rule -> Grid Tern -> Grid Tern
step = extend

render :: Grid Tern-> B.ByteString
render (StoreT (Identity (Compose g)) _) = B.pack $ foldMap ((++ "\n") . foldMap showTern) g

parseFile = V.fromList
            . fmap (V.fromList . fmap toTern . B.unpack)
            . B.lines

toTern c = case c of
             '.' -> Blank
             '>' -> East
             'v' -> South

showTern t = case t of
               Blank -> "."
               East -> ">"
               South -> "v"

notEqual (StoreT (Identity (Compose g)) _)  (StoreT (Identity (Compose g')) _) = g /= g'

main :: IO ()
main = do
  (f:_) <- getArgs :: IO [String]
  image <- parseFile <$> B.readFile f
  let initial = mkGrid image
  let
      gs = iterate (step southRule . step eastRule) initial
      (i,(g,_)) = head . dropWhile (uncurry notEqual . snd) . zip [1..] $ zip gs (tail gs)
  B.putStrLn (render (initial))
  B.putStrLn (render g)
  print i
