module Main where

import Data.Time.Clock

import Types
import qualified UI as UI(main)
import Storage.Serialize

main :: IO ()
main = do
  -- generateTestData

  d <- load

  UI.main d


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