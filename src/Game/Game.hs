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
    
new_game :: String -> Game
new_game str =
    let chars = fromList $ Prelude.map (\x -> (x, 0)) str
    in 
    InProgress str "" 0 chars

is_complete :: Game -> Bool
is_complete (Lost _) = True
is_complete (Won _ _) = True
is_complete (InProgress _ _ guesses chars) 
    | guesses >= 6 = True
    | Data.Map.foldl (+) 0 chars == (Data.Map.size chars) = True
    | otherwise = False

make_guess :: Game -> Char -> Game
make_guess (InProgress word guessed guesses chars) char =
    if char `elem` guessed then
        InProgress word guessed guesses chars
    else
        case Data.Map.lookup char chars of
            Just val -> (
                let updated_game = InProgress word (char : guessed) guesses (adjust (+ 1) char chars)
                in
                case (is_complete updated_game) of
                    True -> Won word guesses
                    False -> updated_game)
            Nothing -> (case guesses == 5 of
                True -> Lost word
                False -> InProgress word (char : guessed) (guesses + 1) chars)


print_game :: Game -> IO ()
print_game (Lost word) = printf "You lost, the word was %s\n" word
print_game (Won word guesses) = printf "You won, the word was %s, and you had %d guesses remaining\n" word (6 - guesses)
print_game (InProgress word guessed guesses characters) =
    let current = Prelude.map (char_conversion characters) word in
    let g = Prelude.foldl (\y x-> concat [y, (x:" ")]) "" guessed
    in
    printf "%-20s -- Guesses remaining: %d -- Characters guessed: %s\n" current (6 - guesses) g

char_conversion :: Characters -> Char -> Char
char_conversion chars x =
    case Data.Map.lookup x chars of
        Just val -> if val == 1 then x else '-'
        Nothing -> '-'

    