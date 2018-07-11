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