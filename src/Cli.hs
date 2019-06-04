module Cli where

import Types

import Data.List (sort)
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
             | a == "start" = clockIn l
             | a == "stop"  = clockOut l
             | a == "list"  = printLog l
             | otherwise    = undefined

printLog :: Log -> IO Log
printLog l = do
  mapM (putStrLn . show) l
  return l

clockIn :: Log -> IO Log
clockIn l = do
  time <- getCurrentTime
  when (not . entryCompleted . mostRecentEntry $ l) $ die "There is still an active entry. Stop it first!"
  putStrLn ("Clocked in at " ++ show time)
  return $ Entry { startTime = Just time
                 , endTime = Nothing
                 , description = "Work work work" } : l

clockOut :: Log -> IO Log
clockOut l = do
  time <- getCurrentTime
  putStrLn ("Clocked out at " ++ show time)
  let cur = mostRecentEntry l
      -- TODO: Isn't there a better way to do this (e.g. like Elm)?
      new = (Entry { startTime = (startTime cur)
                   , endTime = Just time
                   , description = (description cur) })
  return $ replaceEntry l cur new

replaceEntry :: Log -> Entry -> Entry -> Log
replaceEntry (l:ls) o n
    | l == o    = n : replaceEntry ls o n
    | otherwise = l : replaceEntry ls o n

mostRecentEntry :: Log -> Entry
mostRecentEntry l = head $ sort l
