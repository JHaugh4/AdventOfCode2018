module One where

import Data.List (lines, foldl')
import qualified Data.Vector.Unboxed as V
import Data.Int (Int64)
import Data.Monoid (Sum (..))
import Control.Monad.State.Strict

--1
parse :: Int64 -> String -> Int64
parse x ('+':num) = x + read num
parse x ('-':num) = x - read num
parse x line      = error $ "Line " ++ show line ++ " not in right format"

--Monoid version
parseToMon :: String -> Sum Int64
parseToMon (op:num) = case op of
  '+' -> Sum number
  '-' -> Sum $ negate number
  where number = read num

run1 :: IO ()
run1 = do
  file <- readFile "./inputs/One.txt"
  let contents = lines file
  let ans = foldl' parse 0 contents
  let ans' = foldMap parseToMon contents
  print (ans, ans')

--2
loop :: [String] -> Int64
loop contents = evalState (run contents) (0, V.empty)
  where
    run :: [String] -> State (Int64, V.Vector Int64) Int64
    run (l:ls) = do
      (p, prevs) <- get
      let ans = parse p l
      if ans `V.elem` prevs
      then pure ans
      else do
        put (ans, V.cons ans prevs)
        run ls

run2 :: IO ()
run2 = do
  file <- readFile "./inputs/One.txt"
  let contents = lines file
  let ans = loop $ cycle contents
  print ans

--I didn't end up using it but it is an interesting function
foldWhile :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldWhile pred f acc []     = acc
foldWhile pred f acc (x:xs) =
  let interm = f acc x
  in if pred interm
    then foldWhile pred f interm xs
    else interm