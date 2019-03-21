{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Day6 where

import Text.Regex.Applicative.Common
import Common
import Data.List
import Data.Function
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import GHC.Generics (Generic)
import Data.Hashable
import Data.Text.Prettyprint.Doc
import Numeric (showHex)
import Debug.Trace

data Point = Point {
  x :: Int,
  y :: Int
  } deriving (Show, Eq, Generic, Pretty)

instance Hashable Point

instance InputParser Point where
  parseLine = Point <$> decimal <*> (", " *> decimal)

sample :: [Point]
sample =
  [ 
    Point {x = 1, y = 1},
    Point {x = 1, y = 6},
    Point {x = 8, y = 3},
    Point {x = 3, y = 4},
    Point {x = 5, y = 5},
    Point {x = 8, y = 9}
  ]

manhattanDist :: Point -> Point -> Int
manhattanDist (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- to find all points' set of closest points at once, then get the biggest one.
-- filter points which have closest plane points outside of the inPoints!
closestTo points = 
  snd $ maximumBy (compare `on` snd) $
  M.toList $
  (\(sps, dm) -> foldl (inserter dm) M.empty sps) $
  foldl noOut ([], S.empty) $
  map (\sp -> (sp, 
    takeWhileEq $
    sortBy (compare `on` snd) $
    map (\p -> (p, manhattanDist sp p)) points)) $
  Point <$> [lx-1..hx+1] <*> [ly-1..hy+1]
    where
      lx = x $ minimumBy (compare `on` x) points
      ly = y $ minimumBy (compare `on` y) points
      hx = x $ maximumBy (compare `on` x) points
      hy = y $ maximumBy (compare `on` y) points
      outP (Point x y) = x <= lx || x >= hx || y <= ly || y >= hy
      takeWhileEq [] = []
      takeWhileEq [p] = [p]
      takeWhileEq ((p1, d1):(p2, d2):pds) | d1 == d2 = (p1, d1):takeWhileEq ((p2, d2):pds)
                                          | d1 /= d2 = [(p1, d1)]
      noOut (sps, dm) (sp, ps) | outP sp   = (sps, foldl (\dm (p, _) -> S.insert p dm) dm ps)
                               | otherwise = ((sp, ps):sps, dm)
      
      inserter dm m (sp, ps) | any (\p -> S.member p dm) (map fst ps) || length ps > 1 = m
                             | otherwise = M.insertWith (+) (fst $ head ps) 1 m

part1 ps = closestTo ps

part2 within points =
  length $
  filter (\sp -> within > sum (map (manhattanDist sp) points)) $
  Point <$> [lx..hx] <*> [ly..hy]
    where
      lx = x $ minimumBy (compare `on` x) points
      ly = y $ minimumBy (compare `on` y) points
      hx = x $ maximumBy (compare `on` x) points
      hy = y $ maximumBy (compare `on` y) points