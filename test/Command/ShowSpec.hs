module Command.ShowSpec (main, spec) where

import Test.Hspec
import Control.Monad.State
import Command.Show
import Types
import Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Command.Show" $ do
  it "displays the current state correctly" $ do
    let initialState = emptyFix
        (_, output) = runState (execWriterT view) initialState
    -- We will need to capture stdout or similar mechanisms to truly verify
    pendingWith "Implement capture of stdout output"
