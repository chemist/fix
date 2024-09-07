module Command.RenderSpec (main, spec) where

import Test.Hspec
import Control.Monad.State
import Command.Render
import Types
import Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Command.Render" $ do
  it "processes and renders templates" $ do
    let initialState = emptyFix
        action = render
        (_, newState) = runState (execWriterT action) initialState
    -- Compare rendered outputs to expected results
    pendingWith "File rendering needs verification against expected output"
