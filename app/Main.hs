module Main where

import Data.String.Strip
import Game.Game
import Paths_hangman
import System.IO
import System.Random

main :: IO ()
main = do
  fileName <- getDataFileName "words.txt"
  file <- openFile fileName ReadMode
  contents <- hGetContents file
  gen <- getStdGen
  let words = lines contents
  let (index, _) = randomR (0, length words) gen 
  let word = getNthWord index words 
  let game = new_game word
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


getNthWord :: Int -> [String] -> String
getNthWord x = last . take x
