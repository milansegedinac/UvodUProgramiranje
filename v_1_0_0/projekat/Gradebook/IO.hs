module Gradebook.IO
  ( loadGrades
  , saveGrades
  ) where

import Gradebook.Types ( makeGrades, Grades (pupil), toCSV, Pupil (pupilId) )
import Gradebook.Utils (splitOn)
import Control.DeepSeq (deepseq)
import Control.Exception (evaluate)
import Data.List (sortBy)
import Data.Ord (comparing)

gradesFilePath :: FilePath
gradesFilePath = "./Gradebook/data/grades.csv"

loadGrades :: IO [Grades]
loadGrades = do
    content <- readFile gradesFilePath
    _ <- evaluate (length content) -- forces evaluation and releases the file
    return (sortBy (comparing (pupilId . pupil)) (map (makeGrades . splitOn ',') (lines content)))

saveGrades :: [Grades] -> IO ()
saveGrades gradesList =
     writeFile  gradesFilePath (unlines (map toCSV gradesList))

