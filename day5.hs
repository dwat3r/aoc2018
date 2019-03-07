module Day5 where

import Data.Char
import Data.List
import Data.Function

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


part2 = withInput (bestShrink . shrink) "day5.txt"

bestShrink s = 
  length $
  minimumBy (compare `on` length) $
  map shrink $ 
  map (\(c, s) -> 
    filter (\c2 -> c /= c2 && toUpper c /= c2) s) $
  zip ['a'..'z'] $ repeat s