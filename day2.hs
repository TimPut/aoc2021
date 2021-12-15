{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.ByteString.Char8 as B
import Data.Maybe

data Dir = U !Int | D !Int | F !Int
data State = State !Int !Int !Int -- vert horz aim

parse bs = let (op, offset) = case B.head (bs <> "\n") of
                                'f' -> (F,8)
                                'u' -> (U,3)
                                'd' -> (D,5)
                                _ -> (undefined,0)
               mib = (B.readInt $ B.drop offset bs)
           in case mib of
                Just (i,bs') -> op i : parse (B.tail bs')
                Nothing -> []
                
add (State v h a) (F i) = State (v+a*i) (h+i) a
add (State v h a) (U i) = State v h (a-i)
add (State v h a) (D i) = State v h (a+i)

main = do
  xs <- parse <$> B.readFile "./test"
  let (State v h a) = foldl' add (State 0 0 0) xs
  B.putStrLn . B.pack $ show (a*h)
  B.putStrLn . B.pack $ show (v*h)
