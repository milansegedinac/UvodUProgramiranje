module Gradebook.UI
  ( 
       formatGrades
     , showMenu
     , enterGrade
     , options
  ) where

import Gradebook.Types
import Gradebook.IO (loadGrades, saveGrades)
import Gradebook.Logic
  (
    listPupils
   , listSubjects
   , insertGrade
   , getStudentGrades
   , getStudentSubjectGrades, getSubjectGrades
   )


-- formating functions
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

class ToTableData a where
  toTableData :: a -> String

instance ToTableData Pupil where
  toTableData p =
    padRight 3 (show (pupilId p)) ++ " | " ++
    padRight 12 (lastName p) ++ " | " ++
    padRight 10 (firstName p)

instance ToTableData Subject where
  toTableData s = padRight 15 (name s)

instance ToTableData Grades where
  toTableData g =
    toTableData (pupil g) ++ " | " ++
    toTableData (subject g) ++ " | " ++
    padRight 15
      (if null (semesterGrades g)
         then "-"
         else init (concatMap (\x -> show x ++ ",") (semesterGrades g))) ++
    " | " ++
    padRight 5 (maybe "-" show (finalGrade g))

formatPupils :: [Pupil] -> String
formatPupils ps =
  "No  | Last Name    | First Name\n" ++
  replicate 35 '-' ++ "\n" ++
  unlines (map toTableData ps)

formatSubjects :: [Subject] -> String
formatSubjects ss =
  "Subject\n" ++
  replicate 15 '-' ++ "\n" ++
  unlines (map toTableData ss)

formatGrades :: [Grades] -> String
formatGrades gs =
  "No  | Last Name    | First Name | Subject         | Semester Grades | Final\n" ++
  replicate 70 '-' ++ "\n" ++
  unlines (map toTableData gs)

-- user menu
showMenu :: String
showMenu = 
  "\n\
  \Please choose an option:\n\
  \1. All pupils\n\
  \2. All subjects\n\
  \3. Enter grade\n\
  \4. Pupil's grades\n\
  \5. Pupil's grades on a subject\n\
  \6. All grades on a subject\n\
  \7. All grades\n\
  \8. Exit\n\
  \"

-- menu options
enterGrade :: IO (Int, String, Int)
enterGrade = do
    putStrLn "Enter pupil ID:"
    pupilIdStr <- getLine
    putStrLn "Enter subject name:"
    subjectName <- getLine
    putStrLn "Enter grade:"
    gradeStr <- getLine
    return (read pupilIdStr, subjectName, read gradeStr)

-- 
option1 :: IO Bool
option1 = do
  grades <- loadGrades
  putStr (formatPupils (listPupils grades))
  return True

option2 :: IO Bool
option2 = do
  grades <- loadGrades
  putStr (formatSubjects (listSubjects grades))
  return True

option3 :: IO Bool
option3 = do
  grades <- loadGrades                          
  (pupilId, subjectName, newGrade) <- enterGrade 
  saveGrades (insertGrade pupilId subjectName newGrade grades)
  return True

option4 :: IO Bool
option4 = do
  grades <- loadGrades
  putStrLn "Enter pupil ID:"
  pupilIdStr <- getLine
  putStr (formatGrades (getStudentGrades (read pupilIdStr) grades))
  return True

option5 :: IO Bool
option5 = do
  grades <- loadGrades
  putStrLn "Enter pupil ID:"
  pupilIdStr <- getLine
  putStrLn "Enter subject name:"
  subjectName <- getLine
  putStr (formatGrades (getStudentSubjectGrades (read pupilIdStr) subjectName grades))
  return True

option6 :: IO Bool
option6 = do
  grades <- loadGrades
  putStrLn "Enter subject name:"
  subjectName <- getLine
  putStr (formatGrades (getSubjectGrades subjectName grades))
  return True

option7 :: IO Bool
option7 = do
  grades <- loadGrades
  putStr (formatGrades  grades)
  return True

exitProgram :: IO Bool
exitProgram = do
  putStrLn "Exiting..."
  return False

-- Menu list
options :: [IO Bool]
options = [option1, option2, option3, option4, option5, option6, option7, exitProgram]

