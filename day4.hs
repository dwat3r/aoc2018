{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}
module Day4 where

import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Common
import Data.List
import Data.Text.Prettyprint.Doc
import qualified Data.Map as M

data Action = BeginsShift { nr :: Int } | 
              FallsAsleep | 
              WakesUp
            deriving (Eq, Ord, Show, Pretty)

data Data = Data {
  month :: Int,
  day :: Int,
  hour :: Int,
  minute :: Int,
  action :: Action
  } deriving (Eq, Ord, Show, Pretty)

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
