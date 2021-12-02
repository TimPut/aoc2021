import Data.List

data Dir = U Int | D Int | F Int
data Pos = Pos Int Int -- vert horz
  deriving Show

data State = State Int Int Int -- vert horz aim
  deriving Show
parse (c:xs) = let i = read . last . words $ xs in
  case c of
    'f' -> F i
    'u' -> U i
    'd' -> D i
    _ -> undefined
add (Pos v h) (F i) = Pos v (h+i)
add (Pos v h) (U i) = Pos (v-i) h
add (Pos v h) (D i) = Pos (v+i) h

add' (State v h a) (F i) = State (v+a*i) (h+i) a
add' (State v h a) (U i) = State v h (a-i)
add' (State v h a) (D i) = State v h (a+i)


main = do
  xs <- fmap parse . lines <$> readFile "./day2input.txt"
  let (Pos v h) = foldl' add (Pos 0 0) xs
  let (State v' h' a') = foldl' add' (State 0 0 0) xs
  

  putStr "Part one: "
  print (v*h)

  putStr "Part two: "
  print (v'*h')
