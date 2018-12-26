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
  --TODO return random word from file
  let line = getNthWord 4 lines 
  let game = new_game line
  done <- playGame game getLine
  print_game done
  hClose file

playGame :: Game -> IO String -> IO Game
playGame game action = do
  print_game game
  putStrLn "Make a guess: "
  input <- action
  let newGame = make_guess game (head input)
  if is_complete newGame then
    return newGame
  else
    playGame newGame action


getNthWord :: Int -> String -> String
getNthWord x = last . take x . lines
