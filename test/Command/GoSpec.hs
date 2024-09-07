module Command.GoSpec (main, spec) where

import Test.Hspec
import Control.Monad.State
import Command.Go
import Types
import Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Command.Go" $ do
  it "should navigate up the route correctly" $ do
    let initialState = emptyFix
        action = goUp
        (_, newState) = runState (execWriterT action) initialState
    -- Verify the state changes that happen during goUp - placeholder
    pendingWith "Need to simulate the state and filesystem behavior"

  it "should navigate down the route correctly" $ do
    let initialState = emptyFix
        action = goDown
        (_, newState) = runState (execWriterT action) initialState
    -- Verify the state changes that happen during goDown - placeholder
    pendingWith "Need to simulate the state and filesystem behavior"
