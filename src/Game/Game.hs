module Game.Game where

import Data.Map

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
    case Data.Map.lookup char chars of
        Just val -> (
            let updated_game = InProgress word guessed guesses (adjust (+ 1) char chars)
            in
            case (is_complete updated_game) of
                True -> Won word guesses
                False -> updated_game)
        Nothing -> (case guesses == 5 of
            True -> Lost word
            False -> InProgress word (char : guessed) (guesses + 1) chars)
