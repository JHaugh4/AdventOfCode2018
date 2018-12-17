module Four where

import Data.List
import Data.Semigroup
import Data.Monoid
import Control.Monad.State.Strict as S
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Void (Void (..))
import Data.Char
import Debug.Trace
import Control.Lens
import Data.Foldable
import qualified Data.Map.Strict as Map

type Parser = Parsec Void Text.Text

data Line = Line
  { _timestamp :: !Timestamp
  , _action    :: !Action
  } deriving (Eq, Show)

data Timestamp = Timestamp
  { _year    :: !Int
  , _month   :: !Int
  , _day     :: !Int
  , _minutes :: !Int
  } deriving (Eq, Show)

data Action = Asleep
            | Awake
            | Start { guard :: Int }
            deriving (Eq, Show)

makeLenses ''Line
makeLenses ''Timestamp
makePrisms ''Action

instance Ord Line where
  l1 <= l2 = l1^.timestamp <= l2^.timestamp

--Debatable which one is easier to understand
instance Ord Timestamp where
  t1 <= t2 = 
     t1^.year    < t2^.year    || (t1^.year   == t2^.year  && 
    (t1^.month   < t2^.month   || (t1^.month  == t2^.month && 
    (t1^.day     < t2^.day     || (t1^.day    == t2^.day   &&
    (t1^.minutes < t2^.minutes || t1^.minutes == t2^.minutes))))))

-- instance Ord Timestamp where
--   t1 <= t2 =
--     if t1^.year < t2^.year 
--     then True
--     else if t1^.year == t2^.year
--          then if t1^.month < t2^.month
--               then True
--               else if t1^.month == t2^.month
--                    then if t1^.day < t2^.day
--                         then True
--                         else if t1^.day == t2^.day
--                              then if t1^.minutes < t2^.minutes
--                                   then True
--                                   else if t1^.minutes == t2^.minutes
--                                        then True
--                                        else False
--                              else False
--                    else False
--          else False

parseLine :: Parser Line
parseLine = do
  char '['
  y   <- numbers "year"
  char '-'
  m   <- numbers "month"
  char '-'
  d   <- numbers "day"
  space
  numbers "hour"
  char ':'
  min <- numbers "minute"
  char ']'
  space
  act <- asleep <|> awake <|> start
  pure $ Line (Timestamp y m d min) act
  where
    numbers :: String -> Parser Int
    numbers str = read . Text.unpack <$> takeWhileP (Just str) isDigit
    asleep = string "falls asleep" >> pure Asleep
    awake  = string "wakes up" >> pure Awake
    start  = do
      string "Guard"
      space
      char '#'
      n <- numbers "guard numbers"
      space
      string "begins shift"
      pure $ Start n

type GuardNum  = Int
type Minute    = Int
type TimeSlept = Int
type Schedule  = Map.Map Minute TimeSlept

{-This function takes a list of parsed lines as input and folds over it to
  produce a map that tracks how long each guard is asleep for at each minute.-}
trackGuards :: [Line] -> S.State (GuardNum, Map.Map GuardNum Schedule) ()
trackGuards ls = case ls of
  []                                                       -> pure ()
  (Line _ (Start g):ls)                                    -> do
    _1 .= g
    _2 %= Map.insertWith (flip const) g newGuard
    trackGuards ls
  ((Line t1 Asleep):(Line t2 Awake):ls)                    -> do
    curGuard <- use _1
    _2 %= Map.adjust (updateMins (t1^.minutes) (t2^.minutes)) curGuard
    trackGuards ls
  ((Line t1 Asleep):(Line _ (Start g)):(Line t2 Awake):ls) -> do
    curGuard <- use _1
    _1 .= g
    _2 %= Map.adjust (updateMins (t1^.minutes) (t2^.minutes)) curGuard
    trackGuards ls
  where
    updateMins :: Int -> Int -> Schedule -> Schedule
    updateMins from to = 
      Map.mapWithKey (\k x -> if inRange k from to then x+1 else x)
    inRange :: Int -> Int -> Int -> Bool
    inRange i from to = i >= from && i <  to
    newGuard :: Schedule
    newGuard = Map.fromList $ zip [0..59] (repeat 0)

{-These product types allow me to define semigroup instances that use find
  the maximum of a specific field within them. I then use these to fold over
  the output of trackGuards to find the statistics neeeded for the problem.-}
data MinuteTime = MinuteTime
  { _minute :: Int
  , _time   :: Int
  } deriving (Eq, Show)

data GuardTime = GuardTime
  { _guardNum :: Int
  , _minTime  :: MinuteTime
  } deriving (Eq, Show)

data GuardSleep = GuardSleep
  { _guardN  :: Int
  , _asleepT  :: Int
  , _schedule :: Map.Map Int Int
  } deriving (Eq, Show)

makeLenses ''MinuteTime
makeLenses ''GuardTime
makeLenses ''GuardSleep

instance Ord MinuteTime where
  m1 <= m2 = m1^.time <= m2^.time

instance Semigroup MinuteTime where
  (<>) = max

instance Monoid MinuteTime where
  mempty = MinuteTime 0 0

instance Ord GuardTime where
  gt1 <= gt2 = gt1^.minTime <= gt2^.minTime

instance Semigroup GuardTime where
  (<>) = max

instance Monoid GuardTime where
  mempty = GuardTime 0 mempty

instance Ord GuardSleep where
  gs1 <= gs2 = gs1^.asleepT <= gs2^.asleepT

instance Semigroup GuardSleep where
  gs1 <> gs2 = max gs1 gs2

instance Monoid GuardSleep where
  mempty = GuardSleep 0 0 Map.empty

findSleepiestGuard :: Map.Map GuardNum Schedule -> GuardSleep
findSleepiestGuard = 
  Map.foldMapWithKey (\gn m -> GuardSleep gn (sum m) m)

findSleepiestMinute :: Map.Map GuardNum Schedule -> GuardTime
findSleepiestMinute = 
  Map.foldMapWithKey (\gn m -> GuardTime gn $ Map.foldMapWithKey MinuteTime m)

run1 :: IO ()
run1 = do
  file <- Text.readFile "./inputs/Four.txt"
  let contents = traverse (parseMaybe parseLine) (Text.lines file)
  case contents of
    Nothing -> print "parse error!"
    Just ts -> do
      let (_, output) = execState (trackGuards $ sort ts) (0, Map.empty)
      --1
      let sleepiestGuard = findSleepiestGuard output
      print sleepiestGuard
      print $ Map.foldMapWithKey MinuteTime (sleepiestGuard^.schedule)
      --2
      print $ findSleepiestMinute output