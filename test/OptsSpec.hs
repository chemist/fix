module OptsSpec (main, spec) where

import Test.Hspec
import Opts.Opts

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Opts" $ do
  it "should parse options correctly" $ do
    let args = ["add", "--fix-path", "/path/to/fix", "layerName"]
    opts <- execParserPure defaultPrefs (info (helper <*> parseOptions) fullDesc) args
    optFixPath opts `shouldBe` "/path/to/fix"
    optCommand opts `shouldBe` (Command (Add LayerContext) "layerName")
