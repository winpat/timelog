module Main where

import Data.Time.Clock

import Types
import Storage.Serialize
import System.Environment (getArgs)
import Cli (commandLine)

main :: IO ()
main = do
  d <- load
  args <- getArgs
  l <- commandLine args d
  save l



-- Generate some demo data to work with
generateTestData :: IO ()
generateTestData = do
  time <- getCurrentTime

  let e1 = (Entry {startTime = Just time, endTime = Just time, description = "Test data 1"})
  let e2 = (Entry {startTime = Just time, endTime = Just time, description = "Test data 2"})
  let e3 = (Entry {startTime = Just time, endTime = Just time, description = "Test data 3"})
  let e4 = (Entry {startTime = Just time, endTime = Just time, description = "Test data 4"})

  let l = [e1, e2, e3, e4]

  save l
