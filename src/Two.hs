module Two where

import Data.List
import Data.Foldable
import Data.Monoid
import Data.Functor.Const (Const (..))
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.State.Strict

--1
tally :: String -> (Sum Int, Sum Int)
tally line = Map.foldl search (Sum 0, Sum 0) ans
  where
    ans  = execState (traverse_ run line) Map.empty
    search (two, three) x
      | x == 2     = (Sum 1, three)
      | x == 3     = (two, Sum 1)
      | otherwise = (two, three)
    run :: Char -> State (Map.Map Char Int) ()
    run c = do
      mp <- get
      let mp' = Map.insertWith (const (+1)) c 1 mp
      put mp'

run1 :: IO ()
run1 = do
  file <- lines <$> readFile "./inputs/Two.txt"
  let ans = foldMap tally file
  print $ uncurry (*) ans

--2
offBy1 :: String -> String -> Bool
offBy1 str1 str2 = 
  ((== 1) . length) $ foldl' (flip delete) str2 str1

findBox :: [String] -> String -> First (String, String)
findBox strs str = foldMap run strs
  where
    run :: String -> First (String, String)
    run str'
      | offBy1 str str' = First $ Just (str, str')
      | otherwise       = First Nothing

getAns :: (String, String) -> String
getAns (str1, str2) = foldl' (flip delete) str2 str1

run2 :: IO ()
run2 = do
  file <- lines <$> readFile "./inputs/Two.txt"
  let (First (Just ans))  = foldMap (findBox file) file
  let ans' = getAns ans
  print ans