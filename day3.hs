{-# LANGUAGE OverloadedStrings #-}
module Day3 where

import Text.Regex.Applicative()
import Text.Regex.Applicative.Common
import qualified Data.Map as M
import qualified Data.Set as S
import Common

data Data = Data {
  nr :: Int,
  xoff :: Int,
  yoff :: Int,
  xsize :: Int,
  ysize :: Int
  } deriving Show

input :: FilePath 
input = "day3.txt"

instance InputParser Data where
  parseLine = Data <$> ("#"    *> decimal)
                   <*> (" @ "  *> decimal)
                   <*> (","    *> decimal)
                   <*> (": "   *> decimal)
                   <*> ("x"    *> decimal)

overlaps :: Foldable t => t Data -> Int
overlaps s = M.size $ M.filter (>1) $ 
                M.fromListWith (+) $ zip 
                    (concatMap (\d -> (,) <$>   [xoff d .. xoff d + xsize d - 1] <*> 
                                                [yoff d .. yoff d + ysize d - 1]) s) $ 
                    repeat 1

notOverlaps :: [Data] -> Int
notOverlaps = head . S.toList . foldl1 (\nrs1 nrs2 -> nrs1 S.\\ nrs2) . M.elems .
                M.foldl (\m (nrs, c) -> M.insertWith (S.union) c nrs m) M.empty .
                foldl (\m (x, y, n) -> M.insertWith (\(n1,c1) (n2, c2) -> (n1 `S.union` n2, c1 + c2)) (x,y) (S.singleton n, 1) m) M.empty .
                concatMap (\d ->  (,,) <$>  [xoff d .. xoff d + xsize d - 1] <*> 
                                            [yoff d .. yoff d + ysize d - 1] <*>
                                            [nr d])
sample :: [Data]
sample = [
    Data 1 1 3 4 4,
    Data 2 3 1 4 4,
    Data 3 5 5 2 2
    ]

part1 :: IO Int
part1 = withInput overlaps input

part2 :: IO Int
part2 = withInput notOverlaps input
