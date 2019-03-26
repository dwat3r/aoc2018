{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, RecordWildCards #-}
module Day7 where

import Text.Regex.Applicative
import Data.Text.Prettyprint.Doc
import Common
import Data.Char hiding (Space)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Debug.Trace

data Step = Step {
  prev :: Char,
  next :: Char
  } deriving (Show, Eq, Pretty)

instance InputParser Step where
  parseLine = Step <$> 
    ("Step " *> psym isAlpha) <*>
    (" must be finished before step " *> psym isAlpha <* " can begin.")

sample = [ 
    Step {prev = 'C', next = 'A'}
  , Step {prev = 'C', next = 'F'}
  , Step {prev = 'A', next = 'B'}
  , Step {prev = 'A', next = 'D'}
  , Step {prev = 'B', next = 'E'}
  , Step {prev = 'D', next = 'E'}
  , Step {prev = 'F', next = 'E'} ]



part1 input = 
  (\(l, r, m, mr) -> topoSort m mr $ S.difference l r) $
  foldl' buildMap (S.empty, S.empty, M.empty, M.empty) input
    where
      buildMap (lefts, rights, m, mr) Step{..} = 
        ( 
          S.insert prev lefts,
          S.insert next rights,
          M.insertWith (S.union) prev (S.singleton next) m,
          M.insertWith (S.union) next (S.singleton prev) mr
        )
      topoSort m mr avail | S.null avail = []
                          | otherwise = root:(
                              topoSort
                              (M.adjust S.deleteMin root m) 
                              (M.map (S.delete root) mr)
                              (S.delete root $ 
                                if M.member root m 
                                  then S.union (m M.! root) avail
                                  else avail)
                              )
        where
          root = S.findMin $ S.filter (\s -> all S.null $ maybeToList $ mr M.!? s) avail