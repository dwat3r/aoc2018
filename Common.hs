{-# LANGUAGE OverloadedStrings #-}
module Common (
  InputParser,
  parseLine,
  withInput
  )
where

import Text.Regex.Applicative(RE, (=~))
import Data.Maybe(catMaybes)

class InputParser a where
  parseLine :: RE Char a
  withInput :: ([a] -> b) -> FilePath -> IO b
  withInput f input = readFile input
      >>= pure . f . catMaybes . map (=~ parseLine) . lines
