module Cli where

import Types

import Data.List (sort, intercalate)
import Control.Monad (when)
import qualified UI as UI(main)
import System.Exit (die)
import Data.Time.Format
import Data.Time.Clock

commandLine :: [String] -> Log -> IO Log
commandLine args l = do
  let cmd = head args
      arg = intercalate " " $ tail args
  handleArgument cmd arg l

handleArgument :: String -> String -> Log -> IO Log
handleArgument cmd arg l
             | cmd == "ui"    = UI.main l
             | cmd == "start" = clockIn l
             | cmd == "stop"  = clockOut arg l
             | cmd == "list"  = printLog l
             | otherwise      = printLog l

printLog :: Log -> IO Log
printLog l = do
  mapM (putStrLn . renderEntry) l
  return l

renderEntry :: Entry -> String
renderEntry (Entry s e d) = renderDatetime s ++ " - " ++ renderDatetime e ++ ": " ++ d
  where renderDatetime = \x -> case x of
          Nothing -> "In Progress"
          Just d  -> formatTime defaultTimeLocale "%d. %b %H:%M:%S" d

clockIn :: Log -> IO Log
clockIn l = do
  time <- getCurrentTime
  when (not . entryCompleted . mostRecentEntry $ l) $ die "There is still an active entry. Stop it first!"
  putStrLn ("Clocked in at " ++ Types.renderTime time)
  return $ Entry { startTime = Just time
                 , endTime = Nothing
                 , description = "To be specified." } : l

clockOut :: String -> Log -> IO Log
clockOut desc l = do
  time <- getCurrentTime
  let cur = mostRecentEntry l
  when (entryCompleted cur) $ die "The entry has already been completed! Start a new one."
  when (desc == "") $ die "No description specified!"
  let new = Entry (startTime cur) (Just time) desc
  putStrLn ("Clocked out at " ++ Types.renderTime time)
  return $ replaceEntry l cur new

entryCompleted :: Entry -> Bool
entryCompleted e = (endTime e) /= Nothing

replaceEntry :: Log -> Entry -> Entry -> Log
replaceEntry [] o n = []
replaceEntry (x:xs) o n
    | x == o    = n : replaceEntry xs o n
    | otherwise = x : replaceEntry xs o n

mostRecentEntry :: Log -> Entry
mostRecentEntry l = head $ reverse $ sort l
