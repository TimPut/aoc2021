import qualified Data.ByteString.Char8 as B
import Data.List

combine :: Int -> Char -> Int
combine n '1' = n+1
combine n '0' = n

pretty :: [Bool] -> String
pretty = fmap p
  where
    p True = '1'
    p False = '0'

common xs =
  let ones = foldl' (zipWith combine) [0,0..] xs
      zeros = fmap ((-) (length xs)) ones
  in zip ones zeros

b2d = foldl' (\acc bit -> if bit then acc*2 + 1 else acc*2) 0

main = do
  xs <- fmap B.unpack . B.lines <$> B.readFile "./day3input.txt"
  putStrLn . pretty . fmap (uncurry (>)) $ common xs
  putStrLn . pretty . fmap not . fmap (uncurry (>)) $ common xs
  putStrLn $ gas O2 xs
  putStrLn $ gas CO2 xs
  let gamma = b2d . fmap (uncurry (>)) $ common xs
      epsilon = b2d . fmap not . fmap (uncurry (>)) $ common xs
  print (gamma * epsilon)
  let o2 = b2d . fmap (== '1') $ gas O2 xs
      co2 = b2d . fmap (== '1') $ gas CO2 xs
  print (o2 * co2)
        
data Gas = O2 | CO2
  deriving Eq

gas _ [x] = x
gas g (x:xs) = f : (gas g . fmap tail $ keepers)
  where
    (o,z) = head $ common (x:xs)
    f = if (if g == O2
            then (>=)
            else (<))
            o z
        then '1'
        else '0'
    keepers = filter ((==) f . head) (x:xs)
