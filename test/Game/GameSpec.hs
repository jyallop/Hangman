module Game.GameSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Game.Game

main :: IO ()
main = hspec spec

spec :: s
spec = do
    describe "game" $ do 
        it "takes a string and returns Hello ++ string" $ do
            game "Jonathon" `shouldBe` "Hello, Jonathon!"