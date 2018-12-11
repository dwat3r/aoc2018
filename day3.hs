{-# LANGUAGE OverloadedStrings #-}
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Char hiding (Space)

data Data = Data {
  nr :: Int,
  xoff :: Int,
  yoff :: Int,
  xsize :: Int,
  ysize :: Int
  }

-- withInput f = readFile "day3.txt" 
--         >>= (=~ parseLine) . lines
--         >>= f
--   where



parseLine = Data <$> sym '#' *> decimal 
            <* string " @ " *> decimal 
            <* string ","   *> decimal 
            <* string ": "  *> decimal
            <* string "x"   *> decimal
