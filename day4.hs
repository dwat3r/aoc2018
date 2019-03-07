{-# LANGUAGE OverloadedStrings, DeriveAnyClass, RecordWildCards, TemplateHaskell #-}
module Day4 where

import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Common
import Data.List
import Data.Text.Prettyprint.Doc
import qualified Data.Map as M
import Control.Lens

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
succDay d@Data{..} | daysInMonths M.! month == day && month == 12 = Data 1 1 hour minute action
                   | daysInMonths M.! month == day = Data (month + 1) 1 hour minute action
                   | otherwise = Data month (day + 1) hour minute action

removeBeforeMidnight :: [Data] -> [Data]
removeBeforeMidnight = map go
  where
    go d@Data{..} | hour == 23 = succDay (Data month day 0 0 action)
                  | hour >  0  = Data month day 0 59 action
                  | otherwise  = d

intervals = M.map (foldl segment (False, M.empty)) .
  snd . foldl convert (0, M.empty) . sort . 
  removeBeforeMidnight
  where 
    convert (nr1, m) Data{..} = case action of 
      BeginsShift nr2 -> (nr2, M.insertWith (++) nr2 [(month, day, hour, minute)] m)
      FallsAsleep     -> (nr1, M.insertWith (++) nr1 [(month, day, hour, minute)] m)
      WakesUp         -> (nr1, M.insertWith (++) nr1 [(month, day, hour, minute)] m)
    segment (b, m) (mo, d, h, mi) = undefined


    
