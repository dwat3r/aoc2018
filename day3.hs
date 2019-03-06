{-# LANGUAGE OverloadedStrings #-}
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Char hiding (Space)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Debug.Trace

data Data = Data {
  nr :: Int,
  xoff :: Int,
  yoff :: Int,
  xsize :: Int,
  ysize :: Int
  } deriving Show

withInput f = readFile "day3.txt" 
    >>= pure . f . catMaybes . map (=~ parseLine) . lines

overlaps s = M.size $ M.filter (>1) $ 
                M.fromListWith (+) $ zip 
                    (concatMap (\d -> (,) <$>   [xoff d .. xoff d + xsize d - 1] <*> 
                                                [yoff d .. yoff d + ysize d - 1]) s) $ 
                    repeat 1

notOverlaps = head . S.toList . foldl1 (\nrs1 nrs2 -> nrs1 S.\\ nrs2) . M.elems .
                M.foldl (\m (nrs, c) -> M.insertWith (S.union) c nrs m) M.empty .
                foldl (\m (x, y, nr) -> M.insertWith (\(n1,c1) (n2, c2) -> (n1 `S.union` n2, c1 + c2)) (x,y) (S.singleton nr, 1) m) M.empty .
                concatMap (\d ->  (,,) <$>  [xoff d .. xoff d + xsize d - 1] <*> 
                                            [yoff d .. yoff d + ysize d - 1] <*>
                                            [nr d])

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

part2 = withInput notOverlaps
