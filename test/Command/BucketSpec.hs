module Command.BucketSpec (main, spec) where

import Test.Hspec
import Control.Monad.State
import Command.Bucket
import Types
import Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Command.Bucket" $ do
  it "creates and switches to a new bucket" $ do
    let initialState = emptyFix
        action = viewOrCreateOrSwitchBucket "newBucket"
        (_, newState) = runState (execWriterT action) initialState
    rName (stBucket newState) `shouldBe` "newBucket"

  it "lists all buckets correctly" $ do
    pendingWith "This test needs a filesystem mock to check actual directories"
