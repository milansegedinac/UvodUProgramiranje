module Gradebook.Logic
  ( 
     listPupils
    ,listSubjects
    ,insertGrade
    ,getStudentGrades
    ,getStudentSubjectGrades
    ,getSubjectGrades
  ) where

import Gradebook.Types 
import Data.List (nub)


-- 1.	list of all pupils
listPupils :: [Grades] -> [Pupil]
listPupils = nub . map pupil

-- 2.	list of all subjects
listSubjects :: [Grades] -> [Subject]
listSubjects = nub . map subject

-- 3.	input pupil's grade for a subject 
updateGrade :: Int -> String -> Int -> Grades -> Grades
updateGrade pId subjectName newGrade grades =
  if pupilId (pupil grades) == pId && name (subject grades) == subjectName
    then grades { semesterGrades = semesterGrades grades ++ [newGrade] }
    else grades

insertGrade :: Int -> String -> Int -> [Grades] -> [Grades]
insertGrade pupilId subjectName newGrade =
  map (updateGrade pupilId subjectName newGrade)

-- 4.   list of pupil's grades on all subjects
getStudentGrades :: Int -> [Grades] -> [Grades]
getStudentGrades pId = filter (\g -> pupilId (pupil g) == pId)

-- 5.	list of pupil's grades on one subject 
getStudentSubjectGrades :: Int -> String -> [Grades] -> [Grades]
getStudentSubjectGrades pId subjectName =
  filter (\g -> pupilId (pupil g) == pId && name (subject g) == subjectName)

-- 6.	list of all grades on one subject
getSubjectGrades :: String -> [Grades] -> [Grades]
getSubjectGrades subjectName =
  filter (\g -> name (subject g) == subjectName)