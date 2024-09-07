module Command.HelpSpec (main, spec) where

import Test.Hspec
import Control.Monad.State
import Command.Help
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Command.Help" $ do
  it "displays help for templates" $ do
    let (_, output) = runState (execWriterT (showHelp HTemplate)) emptyFix
    -- Ensure the template help text matches expectations
    pendingWith "Verify correct help text is displayed"
