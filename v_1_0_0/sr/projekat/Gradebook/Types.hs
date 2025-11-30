module Gradebook.Types
  ( Pupil(..)
  , Subject(..)
  , Grades(..)
  , makeGrades
  , toCSV
  ) where

import Gradebook.Utils (splitOn)
import Data.List (intercalate)

-- Pupil: pupilId (redni broj), lastName (prezime), firstName (ime)
data Pupil = Pupil
  { pupilId    :: Int
  , lastName   :: String
  , firstName  :: String
  } deriving (Show, Eq)

-- Subject: name (naziv)
data Subject = Subject
  { name :: String
  } deriving (Show, Eq)

-- Grades: pupil, subject, semesterGrades (ocene), finalGrade (zaključna ocena)
data Grades = Grades
  { pupil          :: Pupil
  , subject        :: Subject
  , semesterGrades :: [Int]
  , finalGrade     :: Maybe Int
  } deriving (Show, Eq)

makeGrades :: [String] -> Grades
makeGrades [pupilIdStr, lastName, firstName, subjectName, semesterGradesStr, finalGradeStr] =
  Grades
    { pupil = Pupil
        { pupilId = read pupilIdStr
        , lastName = lastName
        , firstName = firstName
        }
    , subject = Subject { name = subjectName }
    , semesterGrades = map read (splitOn ';' semesterGradesStr)
    , finalGrade = if finalGradeStr == "-" then Nothing else Just (read finalGradeStr)
    }

-- converters to CSV
class ToCSV a where
  toCSV :: a -> String

instance ToCSV Pupil where
  toCSV :: Pupil -> String
  toCSV p =
    show (pupilId p) ++ "," ++
    lastName p ++ "," ++
    firstName p

instance ToCSV Subject where
  toCSV :: Subject -> String
  toCSV = name

instance ToCSV Grades where
  toCSV :: Grades -> String
  toCSV g =
    toCSV (pupil g) ++ "," ++
    toCSV (subject g) ++ "," ++
    intercalate ";" (map show (semesterGrades g)) ++ "," ++
    maybe "-" show (finalGrade g)