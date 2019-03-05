{-# LANGUAGE OverloadedStrings #-}
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Char hiding (Space)
import Data.Maybe
import qualified Data.Map as M

data Data = Data {
  nr :: Int,
  xoff :: Int,
  yoff :: Int,
  xsize :: Int,
  ysize :: Int
  } deriving Show

withInput f = readFile "day3.txt" 
    >>= pure . f . catMaybes . map (=~ parseLine) . lines
        
        -- >>= f
-- --  where

overlaps s = M.size $ M.filter (>1) $ 
                M.fromListWith (+) $ zip 
                    (concatMap (\d -> (,) <$>   [xoff d .. xoff d + xsize d - 1] <*> 
                                                [yoff d .. yoff d + ysize d - 1]) s) $ 
                    repeat 1


parseLine = Data <$> (sym '#' *> decimal)
            <*> (" @ "  *> decimal)
            <*> (","    *> decimal)
            <*> (": "   *> decimal)
            <*> ("x"    *> decimal)

sample = [
    Data 1 1 3 4 4,
    Data 2 3 1 4 4,
    Data 3 5 5 2 2
    ]

part1 = withInput overlaps