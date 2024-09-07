module Command.AddSpec (main, spec) where

import Test.Hspec
import Control.Monad.State
import Command.Add
import Types
import Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Command.Add" $ do
  it "creates a new layer successfully" $ do
    let initialState = emptyFix
        (_, newState) = runState (execWriterT (createLayer "newLayer")) initialState
    (index . rBase . stBucket) newState `shouldBe` "newLayer"

  it "should not create layer with existing name" $ do
    let initialState = (execState (execWriterT (createLayer "layer")) emptyFix)
        (_, newState) = runState (execWriterT (createLayer "layer")) initialState
    length (getAllBuckets newState) `shouldBe` 1
