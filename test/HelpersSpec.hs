module HelpersSpec (main, spec) where

import Test.Hspec
import Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Helpers" $ do
  it "should return default path" $ do
    path `shouldBe` "/Users/chemist/Develop/fix/tmp/"

  it "should return emptyFix" $ do
    let Fix dir bucket state verbosity = emptyFix
    dir `shouldBe` "."
    -- проверка других свойств emptyFix
