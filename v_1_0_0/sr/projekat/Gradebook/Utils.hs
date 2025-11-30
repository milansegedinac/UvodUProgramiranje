module Gradebook.Utils
  ( splitOn
  ) where

splitOn :: Char -> String -> [String]
splitOn c s =
  if dropWhile (== c) s == "" then []
  else
    takeWhile (/= c) s : splitOn c (dropWhile (== c) (dropWhile (/= c) s))
