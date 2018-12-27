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
  let game = newGame word
  done <- playGame game getLine
  printGame done
  hClose file

playGame :: Game -> IO String -> IO Game
playGame game action = do
  printGame game
  putStrLn "Make a guess: "
  input <- action
  let newGame = makeGuess game (head input)
  if isComplete newGame then
    return newGame
  else
    playGame newGame action


getNthWord :: Int -> [String] -> String
getNthWord x = last . take x
