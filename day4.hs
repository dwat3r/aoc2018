{-# LANGUAGE OverloadedStrings, DeriveAnyClass, RecordWildCards, TemplateHaskell #-}
module Day4 where

import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Common
import Data.List
import Data.Function(on)
import Data.Text.Prettyprint.Doc
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Debug.Trace

data Action = BeginsShift { nr :: Int } | 
              FallsAsleep | 
              WakesUp
            deriving (Eq, Ord, Show, Pretty)

makeLenses ''Action

data Data = Data {
  month :: Int,
  day :: Int,
  hour :: Int,
  minute :: Int,
  action :: Action
  } deriving (Eq, Ord, Show, Pretty)

makeLenses ''Data

input :: FilePath
input = "day4.txt"

instance InputParser Data where
  parseLine = Data <$> ("[" *> decimal *> "-" *> decimal)
                   <*> ("-" *> decimal)
                   <*> (" " *> decimal)
                   <*> (":" *> decimal)
                   <*> ("] " *> parseAction)
    where
      parseAction = BeginsShift <$> ("Guard #" *> decimal <*  " begins shift") <|>
                    FallsAsleep <$ "falls asleep" <|>
                    WakesUp <$ "wakes up"

-- parsed with: withInput (pretty . (id :: [Data] -> [Data])) "day4_sample.txt"
sample :: [Data]
sample = 
  [ Data {month = 11, day = 1, hour = 0, minute = 0, action = BeginsShift {nr = 10}}
  , Data {month = 11, day = 1, hour = 0, minute = 5, action = FallsAsleep}
  , Data {month = 11, day = 1, hour = 0, minute = 25, action = WakesUp}
  , Data {month = 11, day = 1, hour = 0, minute = 30, action = FallsAsleep}
  , Data {month = 11, day = 1, hour = 0, minute = 55, action = WakesUp}
  , Data {month = 11, day = 1, hour = 23, minute = 58, action = BeginsShift {nr = 99}}
  , Data {month = 11, day = 2, hour = 0, minute = 40, action = FallsAsleep}
  , Data {month = 11, day = 2, hour = 0, minute = 50, action = WakesUp}
  , Data {month = 11, day = 3, hour = 0, minute = 5, action = BeginsShift {nr = 10}}
  , Data {month = 11, day = 3, hour = 0, minute = 24, action = FallsAsleep}
  , Data {month = 11, day = 3, hour = 0, minute = 29, action = WakesUp}
  , Data {month = 11, day = 4, hour = 0, minute = 2, action = BeginsShift {nr = 99}}
  , Data {month = 11, day = 4, hour = 0, minute = 36, action = FallsAsleep}
  , Data {month = 11, day = 4, hour = 0, minute = 46, action = WakesUp}
  , Data {month = 11, day = 5, hour = 0, minute = 3, action = BeginsShift {nr = 99}}
  , Data {month = 11, day = 5, hour = 0, minute = 45, action = FallsAsleep}
  , Data {month = 11, day = 5, hour = 0, minute = 55, action = WakesUp} ]

daysInMonths :: M.Map Int Int
daysInMonths = M.fromList
  [
    (1, 31),
    (2, 28),
    (3, 31),
    (4, 30),
    (5, 31),
    (6, 30),
    (7, 31),
    (8, 31),
    (9, 30),
    (10, 31),
    (11, 30),
    (12, 31)
  ]

succDay :: Data -> Data
succDay Data{..} | daysInMonths M.! month == day && month == 12 = Data 1 1 hour minute action
                 | daysInMonths M.! month == day = Data (month + 1) 1 hour minute action
                 | otherwise = Data month (day + 1) hour minute action

removeBeforeMidnight :: [Data] -> [Data]
removeBeforeMidnight = map go
  where
    go d@Data{..} | hour == 23 = succDay (Data month day 0 0 action)
                  | hour >  0  = Data month day 0 59 action
                  | otherwise  = d
  
intervals = 
  M.map (M.map (segment (True, 0, M.empty) . tail) . slice) .
  snd . foldl convert (0, M.empty) . sort . 
  removeBeforeMidnight
  where 
    convert (nr1, m) Data{..} = case action of 
      BeginsShift nr2 -> (nr2, M.insertWith (flip (++)) nr2 [(month, day, hour, minute)] m)
      FallsAsleep     -> (nr1, M.insertWith (flip (++)) nr1 [(month, day, hour, minute)] m)
      WakesUp         -> (nr1, M.insertWith (flip (++)) nr1 [(month, day, hour, minute)] m)
    
    slice = M.fromListWith (flip (++)) . map (\(mo, d, _, mi) -> ((mo, d), [mi]))

    segment (b, n, s) [] | n <= 59  = segment (b, n+1, M.insert n b s) []
                         | n == 60  = s
    segment (b, n, s) (mi:ds) | n < mi  = segment (b, n+1, M.insert n b s) (mi:ds)
                              | n == mi = segment (not b, n+1, M.insert n (not b) s) ds

sndMax = maximumBy (compare `on` snd)

nrSleeper =
  fst . sndMax . M.toList .
  M.map (M.foldl (+) 0 . M.map (M.size . M.filter not)) .
  intervals
    
bestTime s = 
  fst $ sndMax $ M.toList $
  M.foldl (M.unionWith (+)) M.empty $ 
  M.map (M.map (const 1) . M.filter not) $
  intervals s M.! nrSleeper s

part1 s = nrSleeper s * bestTime s


part2 =
  (\(nr,(mi, _)) -> nr * mi) .
  maximumBy (compare `on` (snd . snd)) . M.toList .
  M.map (sndMax . M.toList) .
  M.filter (not . M.null) .
  M.map (
    M.foldl (M.unionWith (+)) M.empty .
    M.map (M.map (const 1) . M.filter not)
    ) .
  intervals