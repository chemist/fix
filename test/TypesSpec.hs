module TypesSpec (main, spec) where

import Test.Hspec
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Types" $ do
  it "should convert routeToString from Route" $ do
    routeToString ["foo", "bar"] `shouldBe` "foo.bar"
    routeToString [] `shouldBe` "-"

  it "should initialize Fix with defaults" $ do
    let fix = Fix "." emptyBucket Normal False
    stFixDirectory fix `shouldBe` "."
    -- другие проверки для инициализации

  it "should validate the bucket structure" $ do
    let bucket = emptyBucket { rName = "myBucket" }
    rName bucket `shouldBe` "myBucket"
