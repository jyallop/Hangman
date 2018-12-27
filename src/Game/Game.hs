{-# LANGUAGE TupleSections #-}
module Game.Game where

import Data.Map
import Text.Printf

type FinalWord = String
type Guesses = Int
type Guessed = String
type Characters = Map Char Int

data Game = Won FinalWord Guesses
    | Lost FinalWord
    | InProgress FinalWord Guessed Guesses Characters
    deriving (Show, Eq)
    
newGame :: String -> Game
newGame str =
    let chars = fromList $ Prelude.map (,0) str
    in 
    InProgress str "" 0 chars

isComplete :: Game -> Bool
isComplete (Lost _) = True
isComplete (Won _ _) = True
isComplete (InProgress _ _ guesses chars) 
    | guesses >= 6 = True
    | Data.Map.foldl (+) 0 chars == Data.Map.size chars = True
    | otherwise = False

makeGuess :: Game -> Char -> Game
makeGuess (InProgress word guessed guesses chars) char =
    if char `elem` guessed then
        InProgress word guessed guesses chars
    else
        case Data.Map.lookup char chars of
            Just val -> 
                let updatedGame = InProgress word (char : guessed) guesses (adjust (+ 1) char chars)
                in
                if isComplete updatedGame 
                    then Won word guesses
                    else updatedGame
            Nothing -> if guesses == 5 
                then Lost word
                else InProgress word (char : guessed) (guesses + 1) chars


printGame :: Game -> IO ()
printGame (Lost word) = printf "You lost, the word was %s\n" word
printGame (Won word guesses) = printf "You won, the word was %s, and you had %d guesses remaining\n" word (6 - guesses)
printGame (InProgress word guessed guesses characters) =
    let current = Prelude.map (charConversion characters) word in
    let g = Prelude.foldl (\y x-> y ++ (x : " ")) "" guessed
    in
    printf "%-20s -- Guesses remaining: %d -- Characters guessed: %s\n" current (6 - guesses) g

charConversion :: Characters -> Char -> Char
charConversion chars x =
    case Data.Map.lookup x chars of
        Just val -> if val == 1 then x else '-'
        Nothing -> '-'