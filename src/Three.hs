{-# LANGUAGE OverloadedStrings #-}

module Three where

import Data.List
import Data.Monoid
import Control.Monad.State.Strict
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void (Void (..))
import Debug.Trace

type Parser = Parsec Void Text.Text

data Rect = Rect
  { claimId  :: String
  , topLX    :: Int
  , topLY    :: Int
  , width    :: Int
  , height   :: Int
  , area     :: Int
  , botRX    :: Int
  , botRY    :: Int
  } deriving (Eq, Show)

makeRect :: String -> Int -> Int -> Int -> Int -> Rect
makeRect cid toplx toply w h =
  Rect cid toplx toply w h area botrx botry
  where
    area = w * h
    (botrx, botry) = (toplx+w, toply+h)

emptyRect :: Rect
emptyRect = Rect "" 0 0 0 0 0 0 0

newtype RectIntersect = RectIntersect { getIntersectRect :: Rect }
                        deriving (Eq, Show)

instance Semigroup RectIntersect where
  (RectIntersect r1@(Rect cid1 toplx1 toply1 w1 h1 a1 botrx1 botry1)) <>
    (RectIntersect r2@(Rect cid2 toplx2 toply2 w2 h2 a2 botrx2 botry2))
      | r1 == emptyRect       = RectIntersect r2
      | r2 == emptyRect       = RectIntersect r1
      | not $ doOverlap r1 r2 = RectIntersect emptyRect
      | otherwise             =
        RectIntersect (Rect "" toplxI toplyI wI lI areaI botrxI botryI)
      where
        (toplxI, toplyI) = (max toplx1 toplx2, max toply1 toply2)
        (botrxI, botryI) = (min botrx1 botrx2, min botry1 botry2)
        (wI, lI) = (botrxI - toplxI, botryI - toplyI)
        areaI = wI * lI

instance Monoid RectIntersect where
  mempty = RectIntersect emptyRect

-- newtype RectDifference where = RectDifference { getDifferenceRect :: Rect }
--                                deriving (Eq, Show)

-- instance Semigroup RectDifference where
--   (RectDifference r1@(Rect cid1 toplx1 toply1 w1 h1 a1 botrx1 botry1)) <>
--     (RectDifference r2@(Rect cid2 toplx2 toply2 w2 h2 a2 botrx2 botry2))
--       | r1 == emptyRect       = RectIntersect r2
--       | r2 == emptyRect       = RectIntersect r1
--       | otherwise             =
--         let
--           (rI1, rI2) = (RectIntersect r1, RectIntersect r2)
--           (Rect cid toplx toply w h a botrx botry) = getIntersectRect $ rI1 <> rI2
--         in 
--           RectDifference (Rect "" )

parseLine :: Parser Rect
parseLine = do
  char '#'
  cid <- some numberChar
  space
  char '@'
  space
  toplx <- numbers
  char ','
  topry <- numbers
  char ':'
  space
  w <- numbers
  char 'x'
  h <- numbers
  pure $ makeRect cid toplx topry w h
  where
    numbers = read <$> some numberChar

type Coords = [(Int, Int)]

calcCoords :: Rect -> Coords
calcCoords (Rect cid toplx toply w h a botrx botry) =
  (,) <$> [toplx..(toplx+w-1)] <*> [toply..(toply+h-1)]

foldWithTail :: ([a] -> b -> a -> b) -> b -> [a] -> b
foldWithTail f acc []     = acc
foldWithTail f acc (x:xs) = foldWithTail f (f xs acc x) xs

findOverlap :: [Coords] -> Coords
findOverlap = nub . foldWithTail run []
  where
    run :: [Coords] -> Coords -> Coords -> Coords
    run [] cRest cOn = cRest
    run cs cRest cOn = 
      (foldl' (\ys x -> (cOn `intersect` x) ++ ys) [] cs) ++ cRest

findOverlap' :: [Rect] -> [Rect]
findOverlap' = foldWithTail run []
  where
    run :: [Rect] -> [Rect] -> Rect -> [Rect]
    run [] rRest rOn = rRest
    run rs rRest rOn = 
      (foldl' (\ys x -> 
        let RectIntersect rect = (RectIntersect rOn <> RectIntersect x)
        in rect : ys) [] rs) ++ rRest

-- overlap :: Rect -> Rect -> Rect
-- overlap r1@(Rect cid toplx toply w h a botrx botry) r2@(Rect cid toplx toply w h a botrx botry)
--   | not $ doOverlap r1 r2 = Rect "nil" 0 0 0 0
--   | otherwise             =
--     Rect "" toplxI toplyI (botrxI - toplxI) (botryI - toplyI)
--   where
--     area1        = w1*h1
--     area2        = w2*h2
--     (br1x, br1y) = (frl1+w1, frt1+h1)
--     (br2x, br2y) = (frl2+w2, frt2+h2)
--     (toplxI, toplyI) = (max frl1 frl2, max frt1 frt2)
--     (botrxI, botryI) = (min br1x br2x, min br1y br2y)
--     areaI = (botrxI - toplxI) * (botryI - toplyI)
    -- areaI        = ((min br1x br2x) - (max frl1 frl2)) *
    --                ((min br1y br2y) - (max frt1 frt2))

addAreas :: Rect -> Rect -> Int
addAreas rect1 rect2 = area rect1 + area rect2

doOverlap :: Rect -> Rect -> Bool
doOverlap (Rect cid1 toplx1 toply1 w1 h1 a1 botrx1 botry1) 
  (Rect cid2 toplx2 toply2 w2 h2 a2 botrx2 botry2) =
    (toplx1 < botrx2 && botrx1 > toplx2 &&
     toply1 < botry2 && botry1 > toply2)

findNoOverlap :: [Rect] -> [Rect] -> [Rect]
findNoOverlap ogrs []     = []
findNoOverlap ogrs [r]    = [r]
findNoOverlap ogrs (r:rs) =
  -- | trace (show (r:rs)) False = undefined
  case filter (doOverlap r) ogrs of
    []   -> []
    [r'] -> [r']
    _    -> findNoOverlap ogrs rs

testing :: IO ()
testing = do
  let as = [(10,10), (12,123), (1,3)]
  let xs = [(1,1), (2,2), (3,3)]
  let ys = [(1,2), (2,1), (1,3), (2,2)]
  let zs = [(10,10), (12,12), (2,2), (1,1)]
  print $ findOverlap [zs, xs, as, ys]
  print $ foldl' (\ys' x' -> as `intersect` x' ++ ys') [] [xs, ys, zs]
  print $ foldl' (\ys' x' -> xs `intersect` x' ++ ys') [] [ys, zs]
  print $ foldl' (\ys' x' -> ys `intersect` x' ++ ys') [] [zs]

run1 :: IO ()
run1 = do
  file <- Text.lines <$> Text.readFile "./inputs/Three.txt"
  let rects = traverse (parseMaybe parseLine) file
  case rects of
    Nothing     -> putStrLn "Something went wrong!"
    Just rects' -> 
      print $ findNoOverlap rects' rects'
      --print . length . nub $ calcCoords =<< findOverlap' rects'

testInput :: Text.Text
testInput = "#1 @ 179,662: 16x27"