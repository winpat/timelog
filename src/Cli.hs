module Cli where

import Types

import Control.Monad (when)
import qualified UI as UI(main)
import System.Exit (die)

import Data.Time.Clock


commandLine :: [String] -> Log -> IO Log
commandLine args l = do
  when ((length args) > 1) $ die "Invalid argument specified!"
  let arg = head args
  handleArgument arg l

handleArgument :: String -> Log -> IO Log
handleArgument a l
             | a == "ui"    = UI.main l
             | otherwise    = undefined
