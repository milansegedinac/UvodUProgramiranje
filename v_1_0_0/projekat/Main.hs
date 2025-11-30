module Main where

import Gradebook.UI ( showMenu, options )


-- Main loop
mainLoop :: IO ()
mainLoop = do
  putStrLn showMenu
  input <- getLine
  options !! (read input - 1) >>= \continue ->
    if continue then mainLoop else return ()

main :: IO ()
main = mainLoop