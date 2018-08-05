module Main where

import Data.String.Strip
import Game.Game
import Paths_hangman
import System.IO

main :: IO ()
main = do
  fileName <- getDataFileName "words.txt"
  file <- openFile fileName ReadMode
  lines <- hGetContents file
  let line = getNthWord 4 lines 
  putStrLn line
  print_game (new_game "rad")
  print_game (make_guess (make_guess (new_game "welcome") 'w') 't')
  print_game (Lost "hello")
  print_game (Won "nice work" 4)
  hClose file

getNthWord :: Int -> String -> String
getNthWord x = last . take x . lines
