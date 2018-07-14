module Game.GameSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Map

import Game.Game

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "game" $ do 
        it "Creates a new game" $ do
            new_game "aaaa" `shouldBe` InProgress "aaaa" "" 0 (fromList [('a', 0)])
        it "is_completed on lost game" $ do
            is_complete (Lost "Hello") `shouldBe` True
        it "is_complete on won game" $ do
            is_complete (Won "Rad" 2) `shouldBe` True
        it "is_complete out of guesses" $ do
            is_complete (InProgress "aaaa" "" 6 (fromList [('a', 0)])) `shouldBe` True
        it "is_complete guessed all characters" $ do
            is_complete (InProgress "abab" "wf" 2 (fromList [('a', 1), ('b', 1)])) `shouldBe` True
        it "is_complete on incomplete game" $ do
            is_complete (InProgress "asdf" "wel" 3 (fromList [('a', 1), ('s', 0), ('d', 1), ('f', 0)])) `shouldBe` False
        it "make guess in word" $ do
            make_guess (new_game "welcome") 'e' `shouldBe` InProgress "welcome" "" 0 (fromList [('e', 1), ('w', 0), ('l', 0), ('o', 0), ('m', 0), ('c', 0)])
        it "make guess win" $ do
            make_guess (new_game "aa") 'a' `shouldBe` Won "aa" 0
        it "make guess loss" $ do
            make_guess (InProgress "welcome" "trqpg" 5 (fromList [('e', 1), ('w', 0), ('l', 0), ('o', 0), ('m', 0), ('c', 0)])) 'y' `shouldBe` Lost "welcome"
        it "Make guess not in word" $ do
            make_guess (new_game "rad") 'o' `shouldBe` InProgress "rad" "o" 1 (fromList [('r', 0), ('a', 0), ('d', 0)])