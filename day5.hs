module Day5 where

import Data.Char
import Data.List

withInput :: (String -> b) -> FilePath -> IO b
withInput f input = readFile input >>= pure . f

s1 = "aA"
s2 = "abBA"
s3 = "abAB"
s4 = "aabAAB"
s5 = "dabAcCaCBAcCcaDA"

flipCase c | isUpper c = toLower c
           | isLower c = toUpper c
           | True      = error "wat"

shrink s = 
  let next = shrink' s
  in if next == s
    then s
    else shrink next

shrink' []  = []
shrink' [c] = [c]
shrink' (c1:c2:cs) | flipCase c1 == c2 = shrink' cs
                   | otherwise = c1:shrink' (c2:cs)


part1 = withInput (length . shrink) "day5.txt"


shrink'' = 
--  concat .
  filter (\s -> length s /= 2 || flipCase (s !! 0) /= (s !! 1)) . 
  groupBy (\c1 c2 -> flipCase c1 == c2)