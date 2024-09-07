module CommandSpec (main, spec) where

import Test.Hspec
import Control.Monad.State
import Command
import Helpers -- предположительно, нужен для работы с внутренними состояниями
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Command" $ do
  it "should execute Add LayerContext command" $ do
    let options = Options (Command (Add LayerContext) "testLayer") Normal ""
        initialState = emptyFix
        (_, newState) = runState (execWriterT (execute options)) initialState
    (index . rBase . stBucket) newState `shouldBe` "testLayer"
  
  it "should execute Init command" $ do
    let options = Options (Command Init "") Normal ""
        initialState = emptyFix
        (_, newState) = runState (execWriterT (execute options)) initialState
    stFixDirectory newState `shouldBe` "."
