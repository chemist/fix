module Command.SaveSpec (main, spec) where

import Test.Hspec
import Control.Monad.State
import Command.Save
import Types
import Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Command.Save" $ do
  it "should save current workspace state" $ do
    let initialState = emptyFix
        action = saveWorkSpaceAsLayer
        (_, newState) = runState (execWriterT action) initialState
    -- Check changes in newState reflect saved layers
    pendingWith "Requires mock or real filesystem for complete validation"
