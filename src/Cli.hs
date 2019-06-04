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
  let cur = mostRecentEntry l
  when (entryCompleted cur) $ die "The entry has already been completed! Start a new one."
  let new = updateEndTime cur time
  putStrLn ("Clocked out at " ++ show time)
  return $ replaceEntry l cur new

entryCompleted :: Entry -> Bool
entryCompleted e = (endTime e) /= Nothing

updateEndTime :: Entry -> UTCTime -> Entry
updateEndTime e t = e { endTime = Just t }

replaceEntry :: Log -> Entry -> Entry -> Log
replaceEntry [] o n = []
replaceEntry (x:xs) o n
    | x == o    = n : replaceEntry xs o n
    | otherwise = x : replaceEntry xs o n

mostRecentEntry :: Log -> Entry
mostRecentEntry l = head $ reverse $ sort l
