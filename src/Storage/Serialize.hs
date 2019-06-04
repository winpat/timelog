module Storage.Serialize (load, save) where

import System.Environment (lookupEnv)
import Types

filename = "lambda-time.dat"


dataFile :: IO String
dataFile = do
  path <- lookupEnv "TIMELOG_DATA_FILE"
  return $ case path of
    Nothing -> filename
    Just p -> p


load :: IO Log
load = do
  f <- dataFile
  contents <- readFile f
  -- https://ianthehenry.com/posts/lazy-io/
  seq (length contents) (return ())
  return $ read contents


save :: Log -> IO ()
save ls = do
  f <- dataFile
  writeFile f $ show ls
